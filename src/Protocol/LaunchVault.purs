-- | Launch Vault Implementation - Token launches as a vault with phased liquidity provisioning
-- |
-- | This module reimplements the token launch system using the vault abstraction where:
-- | - Assets: Tokens allocated for launch phases (treasury-controlled)
-- | - Liabilities: Share tokens representing user purchases with term commitments
-- | - Strategy: Phased pricing with ascending tiers and eligibility gating
-- |
-- | Key Features:
-- | - Multi-phase launches with ascending price discovery
-- | - Term commitment requirements enforced through locked shares
-- | - Integration with AMM pools for liquidity provisioning
-- | - Automatic position creation with duration-based locking
-- |
-- | Launch Flow:
-- | 1. Treasury deposits launch tokens into vault
-- | 2. Vault provisions liquidity to AMM in phases (Monthly, Spot)
-- | 3. Users purchase tokens through vault, receiving locked shares
-- | 4. Shares unlock based on term commitments (Monthly = 30 days)
-- | 5. After unlock, users can withdraw their tokens
module Protocol.LaunchVault
  ( -- Core types
    LaunchVault
  , LaunchStrategy
  , LaunchPhase(..)
  , PhaseConfig
  , LaunchConfig
  , LaunchStatus
  , GatedSwapParams
  , GatedSwapResult
  -- Vault creation
  , createLaunchVault
  , initializeLaunchVault
  -- Balance sheet operations
  , launchSharePrice
  -- State management
  , checkPhaseComplete
  , transitionPhase
  , getLaunchStatus
  , canParticipate
  -- Functions
  , provisionPhase
  , purchaseTokens
  , claimTokens
  , gatedSwap
  , validateSwapEligibility
  , processLaunchSwap
  , launchAllocationStrategy
  ) where

import Prelude
import Effect (Effect)
import Effect.Ref (Ref, new, read, write, modify_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Array ((:), find, filter)
import Data.Foldable (sum, foldr)
import Data.Int (toNumber)
import Unsafe.Coerce (unsafeCoerce)
import Protocol.Vault 
  ( Vault, VaultState, BalanceSheet, Asset, Liability, ShareAmount
  , deposit, withdraw, requestWithdraw, processWithdrawRequest
  , allocate, totalAssets, defaultSharePrice
  )
import Protocol.Token (TokenType(..))
import Protocol.Common (BlockNumber, PoolId, PositionId)
import Protocol.PositionVault (Duration(..), Leverage(..), Position, createPosition)
import Protocol.Error (ProtocolError(..))
import Protocol.Pool (PoolState, addLiquidity, LiquidityParams, SwapParams, SwapResult, swap)
import FFI (currentTime)
import Data.Ord (abs)

--------------------------------------------------------------------------------
-- SECTION 1: BALANCE SHEET
--------------------------------------------------------------------------------

-- | Sequential phases of token launch with increasing prices
data LaunchPhase
  = MonthlyPhase   -- Phase 1: Lowest prices, requires monthly commitment
  | SpotPhase      -- Phase 2: Higher prices, no commitment required
  | Completed      -- Final state: Launch concluded

derive instance eqLaunchPhase :: Eq LaunchPhase
derive instance ordLaunchPhase :: Ord LaunchPhase

instance showLaunchPhase :: Show LaunchPhase where
  show MonthlyPhase = "Monthly"
  show SpotPhase = "Spot"
  show Completed = "Completed"

-- | Configuration for individual launch phases
type PhaseConfig =
  { phase :: LaunchPhase
  , tokenAmount :: Number         -- Tokens allocated to this phase
  , priceRangeLower :: Number     -- Lower price bound
  , priceRangeUpper :: Number     -- Upper price bound
  , tickLower :: Int              -- AMM tick boundaries
  , tickUpper :: Int
  , lockDuration :: Int           -- Lock duration in blocks
  }

-- | Complete launch configuration
type LaunchConfig =
  { tokenTicker :: String         -- Token symbol
  , totalTokens :: Number         -- Total tokens for launch
  , phases :: Array PhaseConfig   -- Phase configurations
  , treasuryAddress :: String     -- Treasury receiving proceeds
  }

-- | Launch status for queries
type LaunchStatus =
  { phase :: LaunchPhase
  , tokensRemaining :: Number
  , currentPrice :: Number
  , totalRaised :: Number
  , isActive :: Boolean
  }

-- | Parameters for gated swaps during launch
type GatedSwapParams =
  { swapParams :: SwapParams        -- Standard AMM swap parameters
  , user :: String                  -- User address performing the swap
  , duration :: Duration            -- User's chosen position duration
  , poolId :: String                -- Pool identifier for the swap
  }

-- | Result of gated swap execution
type GatedSwapResult =
  { swapResult :: SwapResult        -- Standard AMM swap execution result
  , position :: Maybe Position      -- Position created (only for token purchases)
  }

-- | Launch share price based on phase pricing
-- | Uses phase-specific pricing tiers to determine share value
launchSharePrice :: LaunchStrategy -> BalanceSheet -> Number
launchSharePrice strategy bs =
  case strategy.currentPhase of
    MonthlyPhase -> 0.15   -- Discounted price for monthly commitment
    SpotPhase -> 0.30      -- Standard price for spot purchases
    Completed -> defaultSharePrice bs  -- Market-based pricing after launch

--------------------------------------------------------------------------------
-- SECTION 2: STATE
--------------------------------------------------------------------------------

-- | Launch-specific strategy state
type LaunchStrategy =
  { config :: LaunchConfig
  , currentPhase :: LaunchPhase
  , phaseStartBlock :: BlockNumber
  , tokensProvisioned :: Number   -- Tokens sent to AMM
  , tokensSold :: Number          -- Tokens purchased by users
  , totalRaised :: Number         -- FeelsSOL raised
  , poolId :: Maybe PoolId        -- Associated AMM pool
  , monthlyLockBlocks :: Int      -- Lock duration for monthly phase (e.g., 216000 ≈ 30 days)
  }

-- | Launch vault type
type LaunchVault = Vault LaunchStrategy

-- | Get current launch status
getLaunchStatus :: Ref LaunchVault -> Effect LaunchStatus
getLaunchStatus vaultRef = do
  vault <- read vaultRef
  let strategy = vault.state.strategyState
      tokensRemaining = strategy.tokensProvisioned - strategy.tokensSold
      currentPrice = launchSharePrice strategy vault.state.balanceSheet
      isActive = strategy.currentPhase /= Completed && tokensRemaining > 0.0
      
  pure 
    { phase: strategy.currentPhase
    , tokensRemaining: tokensRemaining
    , currentPrice: currentPrice
    , totalRaised: strategy.totalRaised
    , isActive: isActive
    }

-- | Check participation eligibility
canParticipate :: LaunchPhase -> Duration -> Boolean
canParticipate phase duration = case phase, duration of
  MonthlyPhase, Monthly -> true
  SpotPhase, _ -> true
  Completed, _ -> false
  _, _ -> false

-- | Check if current phase is complete
checkPhaseComplete :: Ref LaunchVault -> Effect Boolean
checkPhaseComplete vaultRef = do
  vault <- read vaultRef
  let strategy = vault.state.strategyState
  
  case findPhaseConfig strategy.config strategy.currentPhase of
    Nothing -> pure false
    Just phaseConfig ->
      -- Phase complete when allocation is sold
      pure $ strategy.tokensSold >= strategy.tokensProvisioned

-- | Transition to next phase
transitionPhase :: Ref LaunchVault -> Effect (Either ProtocolError LaunchPhase)
transitionPhase vaultRef = do
  vault <- read vaultRef
  let strategy = vault.state.strategyState
  
  -- Determine next phase
  let nextPhase = case strategy.currentPhase of
        MonthlyPhase -> SpotPhase
        SpotPhase -> Completed
        Completed -> Completed
        
      updatedStrategy = strategy { currentPhase = nextPhase }
      updatedState = vault.state { strategyState = updatedStrategy }
  
  modify_ (\v -> v { state = updatedState }) vaultRef
  pure $ Right nextPhase

--------------------------------------------------------------------------------
-- SECTION 3: FUNCTIONS
--------------------------------------------------------------------------------

-- | Create a new launch vault
createLaunchVault :: String -> LaunchStrategy -> Effect (Ref LaunchVault)
createLaunchVault name initialStrategy = do
  -- Initial balance sheet with treasury tokens
  let initialBalanceSheet =
        { assets: [{ tokenType: Token initialStrategy.config.tokenTicker
                   , amount: initialStrategy.config.totalTokens
                   , venue: "Treasury" }]
        , liabilities: []
        , totalShares: 0.0
        }
      
      -- Initial vault state
      initialState =
        { balanceSheet: initialBalanceSheet
        , strategyState: initialStrategy
        , lastUpdateBlock: 0
        }
      
      -- Launch-specific share price function
      launchVaultSharePrice :: BalanceSheet -> Number
      launchVaultSharePrice = launchSharePrice initialStrategy
  
  -- Create a ref to hold the vault
  vaultRef <- new (unsafeCoerce unit :: LaunchVault)
  
  -- Create vault implementation
  let vault =
        { name: name
        , state: initialState
        
        -- Deposit implementation (treasury deposits launch tokens)
        , deposit: \tokenType amount depositor -> do
            currentVault <- read vaultRef
            let expectedToken = Token initialStrategy.config.tokenTicker
            case tokenType of
              t | t == expectedToken -> do
                let result = deposit launchVaultSharePrice currentVault.state tokenType amount depositor
                modify_ (\v -> v { state = result.state }) vaultRef
                pure result.shares
              _ -> pure 0.0  -- Only accept launch token deposits
        
        -- Withdraw implementation (users claim tokens after unlock)
        , withdraw: \shares withdrawer -> do
            currentVault <- read vaultRef
            case withdraw launchVaultSharePrice currentVault.state shares withdrawer of
              Nothing -> pure 0.0
              Just result -> do
                modify_ (\v -> v { state = result.state }) vaultRef
                pure result.amount
        
        -- Allocation strategy (provisions liquidity to AMM)
        , allocate: do
            currentVault <- read vaultRef
            let newState = allocate currentVault.state launchAllocationStrategy
            modify_ (\v -> v { state = newState }) vaultRef
        
        -- Strategy update
        , updateStrategy: \newStrategy -> do
            modify_ (\v -> v { state = v.state { strategyState = newStrategy } }) vaultRef
        
        -- Share price implementation
        , sharePrice: launchVaultSharePrice
        }
  
  -- Write the vault to the ref
  _ <- write vault vaultRef
  pure vaultRef

-- | Initialize a new launch vault
initializeLaunchVault :: LaunchConfig -> Effect (Either ProtocolError (Ref LaunchVault))
initializeLaunchVault config = do
  -- Validate phase allocations
  let phaseTotal = sum $ map _.tokenAmount config.phases
      tolerance = 0.01
      
  if abs (phaseTotal - config.totalTokens) > tolerance
    then pure $ Left $ InvalidCommandError $ 
      "Phase amounts don't match total tokens"
    else do
      let initialStrategy =
            { config: config
            , currentPhase: MonthlyPhase
            , phaseStartBlock: 0
            , tokensProvisioned: 0.0
            , tokensSold: 0.0
            , totalRaised: 0.0
            , poolId: Nothing
            , monthlyLockBlocks: 216000  -- ~30 days at 12s blocks
            }
      vaultRef <- createLaunchVault ("Launch-" <> config.tokenTicker) initialStrategy
      pure $ Right vaultRef

-- | Provision tokens to AMM for current phase
provisionPhase :: Ref LaunchVault -> PoolState -> BlockNumber -> Effect (Either ProtocolError Unit)
provisionPhase vaultRef pool currentBlock = do
  vault <- read vaultRef
  let strategy = vault.state.strategyState
  
  -- Find phase configuration
  case findPhaseConfig strategy.config strategy.currentPhase of
    Nothing -> pure $ Left $ InvalidCommandError "Invalid phase"
    Just phaseConfig -> do
      -- Move assets from Treasury to AMM
      let updatedAssets = map (\asset ->
            if asset.venue == "Treasury"
            then asset { amount = asset.amount - phaseConfig.tokenAmount }
            else asset
          ) vault.state.balanceSheet.assets
          
          -- Add AMM allocation
          ammAsset = 
            { tokenType: Token strategy.config.tokenTicker
            , amount: phaseConfig.tokenAmount
            , venue: "AMM-Pool-" <> fromMaybe "" strategy.poolId
            }
          
          finalAssets = ammAsset : filter (\a -> a.amount > 0.0) updatedAssets
          updatedBalanceSheet = vault.state.balanceSheet { assets = finalAssets }
          
          -- Update strategy
          updatedStrategy = strategy 
            { phaseStartBlock = currentBlock
            , tokensProvisioned = strategy.tokensProvisioned + phaseConfig.tokenAmount
            }
          
          updatedState = vault.state 
            { balanceSheet = updatedBalanceSheet
            , strategyState = updatedStrategy
            , lastUpdateBlock = currentBlock
            }
      
      modify_ (\v -> v { state = updatedState }) vaultRef
      
      -- Inject liquidity into AMM
      let liquidityParams = 
            { tickLower: phaseConfig.tickLower
            , tickUpper: phaseConfig.tickUpper
            , amount: phaseConfig.tokenAmount
            , recipient: "launch-vault"
            }
      _ <- pure $ addLiquidity pool liquidityParams
      
      pure $ Right unit

-- | Purchase tokens during launch (creates locked shares)
purchaseTokens :: 
  Ref LaunchVault -> 
  String ->           -- User address
  Number ->           -- FeelsSOL amount
  Duration ->         -- Commitment duration
  BlockNumber ->      -- Current block
  Effect (Either ProtocolError ShareAmount)
purchaseTokens vaultRef user feelsAmount duration currentBlock = do
  vault <- read vaultRef
  let strategy = vault.state.strategyState
  
  -- Validate participation eligibility
  if not (canParticipate strategy.currentPhase duration)
    then pure $ Left $ InvalidCommandError $ 
      "Duration " <> show duration <> " not eligible for " <> show strategy.currentPhase
    else do
      -- Calculate tokens to allocate based on phase price
      let sharePrice = launchSharePrice strategy vault.state.balanceSheet
          tokensToAllocate = feelsAmount / sharePrice
          
      -- Find phase config for lock duration
      case findPhaseConfig strategy.config strategy.currentPhase of
        Nothing -> pure $ Left $ InvalidCommandError "Invalid phase"
        Just phaseConfig -> do
          -- Create locked liability with maturity based on phase
          let maturityBlock = currentBlock + phaseConfig.lockDuration
              
              newLiability = 
                { holder: user
                , shares: tokensToAllocate
                , depositBlock: currentBlock
                , maturityBlock: Just maturityBlock
                , locked: true
                }
              
              -- Update balance sheet
              updatedLiabilities = newLiability : vault.state.balanceSheet.liabilities
              updatedTotalShares = vault.state.balanceSheet.totalShares + tokensToAllocate
              
              updatedBalanceSheet = vault.state.balanceSheet
                { liabilities = updatedLiabilities
                , totalShares = updatedTotalShares
                }
              
              -- Update strategy tracking
              updatedStrategy = strategy
                { tokensSold = strategy.tokensSold + tokensToAllocate
                , totalRaised = strategy.totalRaised + feelsAmount
                }
              
              updatedState = vault.state
                { balanceSheet = updatedBalanceSheet
                , strategyState = updatedStrategy
                , lastUpdateBlock = currentBlock
                }
          
          modify_ (\v -> v { state = updatedState }) vaultRef
          pure $ Right tokensToAllocate

-- | Claim tokens after lock period expires
claimTokens ::
  Ref LaunchVault ->
  String ->           -- User address
  ShareAmount ->      -- Shares to redeem
  BlockNumber ->      -- Current block
  Effect (Either ProtocolError Number)
claimTokens vaultRef user shares currentBlock = do
  vault <- read vaultRef
  
  -- Find user's liability
  case find (\l -> l.holder == user) vault.state.balanceSheet.liabilities of
    Nothing -> pure $ Left $ InvalidCommandError "No position found"
    Just liability ->
      case liability.maturityBlock of
        Nothing -> do
          -- No lock, immediate withdrawal
          amount <- vault.withdraw shares user
          pure $ Right amount
        Just maturity ->
          if currentBlock >= maturity
          then do
            -- Lock expired, process withdrawal
            amount <- vault.withdraw shares user
            pure $ Right amount
          else
            pure $ Left $ InvalidCommandError $ 
              "Tokens locked until block " <> show maturity

-- | Execute swap with launch phase restrictions and term commitment validation
gatedSwap :: 
  PoolState -> 
  Maybe (Ref LaunchVault) -> 
  GatedSwapParams -> 
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
gatedSwap pool maybeLaunch params currentBlock = do
  case maybeLaunch of
    -- No active launch - execute standard AMM swap
    Nothing -> do
      let swapResultWithPool = swap pool params.swapParams currentBlock
          swapResult = swapResultWithPool.result
      pure $ Right { swapResult, position: Nothing }
      
    -- Launch exists - check activation and eligibility
    Just launchRef -> do
      vault <- read launchRef
      let strategy = vault.state.strategyState
          isActive = strategy.currentPhase /= Completed && 
                    (strategy.tokensProvisioned - strategy.tokensSold) > 0.0
      
      if not isActive
        then do
          -- Launch exists but phase inactive - standard swap
          let swapResultWithPool = swap pool params.swapParams currentBlock
              swapResult = swapResultWithPool.result
          pure $ Right { swapResult, position: Nothing }
        else do
          -- Active launch phase - validate user eligibility
          eligibility <- validateSwapEligibility strategy params
          case eligibility of
            Left err -> pure $ Left err
            Right _ -> processLaunchSwap pool launchRef params currentBlock

-- | Validate user eligibility for participation in current launch phase
validateSwapEligibility :: 
  forall r. 
  { currentPhase :: LaunchPhase | r } -> 
  GatedSwapParams -> 
  Effect (Either ProtocolError Unit)
validateSwapEligibility launch params = do
  -- Only gate FeelsSOL → Token swaps (token purchases)
  -- Token → FeelsSOL swaps (selling) are always permitted
  if not params.swapParams.zeroForOne
    then pure $ Right unit  -- Token selling unrestricted
    else
      -- Validate term commitment meets phase requirements
      if canParticipate launch.currentPhase params.duration
        then pure $ Right unit
        else pure $ Left $ InvalidCommandError $ 
          "Duration " <> show params.duration <> 
          " not eligible for " <> show launch.currentPhase <> " phase"

-- | Process swap execution during active launch with position creation
processLaunchSwap ::
  PoolState ->
  Ref LaunchVault ->
  GatedSwapParams ->
  BlockNumber ->
  Effect (Either ProtocolError GatedSwapResult)
processLaunchSwap pool launchRef params currentBlock = do
  -- Execute AMM swap with standard mechanics
  let swapResultWithPool = swap pool params.swapParams currentBlock
      swapResult = swapResultWithPool.result
      updatedPool = swapResultWithPool.updatedPool
  
  -- Create position for token purchases (FeelsSOL → Token swaps)
  if params.swapParams.zeroForOne && swapResult.amount1 > 0.0
    then do
      -- Generate position for purchased tokens with user's term commitment
      let positionId = 1  -- Would be derived from Program Derived Addresses in Solana
          position = createPosition
            positionId
            params.user              -- Position owner
            swapResult.amount1       -- Initial investment amount
            1.0                      -- Price level (default for launches)
            params.duration          -- Time commitment
            Junior                   -- Junior leverage for launch participants
            pool.token0              -- Lend asset (token being launched)
            pool.token1              -- Collateral asset (FeelsSOL)
            false                    -- No rollover for launch positions
            swapResult.amount1       -- Initial shares equal token amount
            currentBlock
      
      -- Update launch tracking in vault
      vault <- read launchRef
      let strategy = vault.state.strategyState
          updatedStrategy = strategy
            { tokensSold = strategy.tokensSold + swapResult.amount1
            , totalRaised = strategy.totalRaised + abs swapResult.amount0
            }
          updatedState = vault.state { strategyState = updatedStrategy }
      
      modify_ (\v -> v { state = updatedState }) launchRef
        
      pure $ Right { swapResult, position: Just position }
    else
      -- Non-purchase swaps don't create positions
      pure $ Right { swapResult, position: Nothing }

-- | Launch allocation strategy for liquidity provisioning
launchAllocationStrategy :: LaunchStrategy -> Array Asset -> Array Asset
launchAllocationStrategy strategy assets =
  -- This strategy maintains assets in their current venues
  -- Real allocation happens through provisionPhase
  assets

--------------------------------------------------------------------------------
-- UTILITY FUNCTIONS
--------------------------------------------------------------------------------

-- | Find phase configuration
findPhaseConfig :: LaunchConfig -> LaunchPhase -> Maybe PhaseConfig
findPhaseConfig config phase = 
  find (\p -> p.phase == phase) config.phases