module UI 
  ( renderUI
  , initialState
  ) where

import Prelude
import Token (TokenType(..), TokenAmount, TokenMetadata, TokenCreationParams, createToken, launchToken, isValidTicker, isTradeable)
import Position (Position, PositionParams, defaultParams, describePosition, priceToTick, tickToPrice)
import Risk (getRiskAssessment, formatRiskAssessment, calculatePositionFee)
import System (SystemParams, defaultSystemParams)
import Pool (PoolState, initPool, addLiquidity, swap, getPoolTVL, token0FromLiquidity, token1FromLiquidity)
import Utils (find, filter, mapArray, formatPrice, formatFeePercentage)
import Data.Array (fold, length, null, sortBy)
import Data.Foldable (sequence_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (compare)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode, toParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Web.DOM.Element (Element)

-- Foreign function imports from FFI module
import FFI (setInnerHTML, addEventListener, removeAllEventListeners, getValue, parseFloat, setTimeout, toNumber, floor, setGlobalState, getGlobalState)

-- Application state
type AppState = 
  { pool :: Maybe PoolState           -- Active liquidity pool
  , selectedToken :: TokenType        -- Currently selected token
  , selectedPair :: TokenType         -- Selected pair token
  , tokens :: Array TokenMetadata     -- All created tokens
  , balance :: { sol :: Number, tokens :: Array { ticker :: String, amount :: Number } }  -- User balances
  , inputAmount :: Number             -- Amount to use for position
  , selectedParams :: PositionParams  -- Parameters for position creation
  , currentPrice :: Number            -- Current tick price for selected pair
  , priceRange :: { min :: Number, max :: Number }  -- Price range for position
  , selectedPositionId :: Maybe Int   -- Selected position for editing
  , actionType :: String              -- "create" | "change" | "exit" | "createToken" | "launchToken"
  , tokenCreation :: { ticker :: String, name :: String }  -- Token creation form data
  }

-- Initial state
initialState :: AppState
initialState =
  { pool: Nothing
  , selectedToken: SyntheticSOL
  , selectedPair: Token "DEFAULT"
  , tokens: []
  , balance: { sol: 1000.0, tokens: [] }
  , inputAmount: 100.0
  , selectedParams: defaultParams (Token "DEFAULT")
  , currentPrice: 1.0
  , priceRange: { min: 0.5, max: 2.0 }
  , selectedPositionId: Nothing
  , actionType: "create"
  , tokenCreation: { ticker: "", name: "" }
  }

-- Note: formatFeePercentage is now imported from Utils module


-- Get balance for token type
getBalance :: AppState -> TokenType -> Number
getBalance state token = case token of
  SyntheticSOL -> state.balance.sol
  Token ticker -> 
    case find (\t -> t.ticker == ticker) state.balance.tokens of
      Just tokenBalance -> tokenBalance.amount
      Nothing -> 0.0
  _ -> 0.0

-- Note: formatPrice is now imported from Utils module

-- Generate orderbook display
generateOrderbook :: AppState -> String
generateOrderbook state = case state.pool of
  Nothing -> "<div class=\"no-pool\">No pool initialized. Create the first position to initialize.</div>"
  Just pool -> 
    let positions = sortBy (\a b -> compare (tickToPrice a.params.tickLower) (tickToPrice b.params.tickLower)) pool.positions
        currentTick = pool.currentTick
    in """
    <div class="orderbook">
      <div class="orderbook-header">
        <h3>Tick Positions</h3>
        <div class="pool-stats">
          <span>Current Price: """ <> formatPrice pool.currentPrice <> """</span>
          <span>Total Liquidity: """ <> formatPrice pool.liquidity <> """</span>
        </div>
      </div>
      <div class="orderbook-grid">
        <div class="orderbook-labels">
          <span>Price Range</span>
          <span>Liquidity</span>
          <span>Token Amounts</span>
          <span>Parameters</span>
          <span>Fee</span>
          <span>Owner</span>
        </div>
        """ <> fold (map (generateOrderbookEntry pool currentTick) positions) <> """
      </div>
    </div>
    """

-- Generate individual orderbook entry
generateOrderbookEntry :: PoolState -> Int -> Position -> String
generateOrderbookEntry pool currentTick position =
  let priceLower = tickToPrice position.params.tickLower
      priceUpper = tickToPrice position.params.tickUpper
      amount0 = token0FromLiquidity position.liquidity currentTick position.params.tickLower position.params.tickUpper
      amount1 = token1FromLiquidity position.liquidity currentTick position.params.tickLower position.params.tickUpper
      inRange = currentTick >= position.params.tickLower && currentTick < position.params.tickUpper
      rangeClass = if inRange then " in-range" else ""
      fee = calculatePositionFee position.params defaultSystemParams
  in """
  <div class="orderbook-entry""" <> rangeClass <> """" data-position-id="""" <> show position.id <> """">
    <span class="price-range">""" <> formatPrice priceLower <> """ - """ <> formatPrice priceUpper <> """</span>
    <span class="liquidity">""" <> formatPrice position.liquidity <> """</span>
    <span class="amounts">
      <div>SOL: """ <> formatPrice amount0 <> """</div>
      <div>""" <> show pool.token1 <> """: """ <> formatPrice amount1 <> """</div>
    </span>
    <span class="params">
      <div>Range: """ <> show position.params.tickLower <> " - " <> show position.params.tickUpper <> """</div>
      <div>Duration: """ <> show position.params.duration <> """d</div>
      <div>Leverage: """ <> show position.params.leverageRate <> """x</div>
    </span>
    <span class="fee">""" <> formatFeePercentage fee <> """</span>
    <span class="owner">""" <> position.owner <> """</span>
  </div>
  """

-- Generate control panel
generateControlPanel :: AppState -> String
generateControlPanel state = """
<div class="control-panel">
  <h2>Position Management</h2>
  
  <div class="balance-display">
    <h3>Your Balances</h3>
    <div class="balances">
      <div class="balance-item">
        <span>SOL:</span>
        <strong>""" <> show state.balance.sol <> """</strong>
      </div>""" <> 
      fold (mapArray renderTokenBalance state.balance.tokens) <> """
    </div>
  </div>
  
  <div class="action-selector">
    <button class="action-btn""" <> (if state.actionType == "create" then " active" else "") <> """" data-action="create">Create Position</button>
    <button class="action-btn""" <> (if state.actionType == "change" then " active" else "") <> """" data-action="change">Change Position</button>
    <button class="action-btn""" <> (if state.actionType == "exit" then " active" else "") <> """" data-action="exit">Exit Position</button>
  </div>
  
  <div class="position-inputs">
    <h3>Position Parameters</h3>
    
    <div class="input-group">
      <label for="pairSelect">Token Pair:</label>
      <select id="pairSelect">
""" <> generateTokenOptions state <> """
      </select>
    </div>
    
    <div class="input-group">
      <label for="amount">Amount (""" <> show state.selectedToken <> """):</label>
      <input type="number" id="amount" value="""" <> show state.inputAmount <> """" min="0.01" step="0.01">
      <small>Available: """ <> show (getBalance state state.selectedToken) <> """</small>
    </div>
    
    <div class="price-range-inputs">
      <h4>Price Range</h4>
      <div class="range-grid">
        <div class="input-group">
          <label for="minPrice">Min Price:</label>
          <input type="number" id="minPrice" value="""" <> show state.priceRange.min <> """" min="0.0001" step="0.0001">
          <small>Tick: """ <> show (priceToTick state.priceRange.min) <> """</small>
        </div>
        <div class="input-group">
          <label for="maxPrice">Max Price:</label>
          <input type="number" id="maxPrice" value="""" <> show state.priceRange.max <> """" min="0.0001" step="0.0001">
          <small>Tick: """ <> show (priceToTick state.priceRange.max) <> """</small>
        </div>
      </div>
    </div>
    
    <div class="unified-params">
      <h4>Unified Parameters</h4>
      <div class="param-grid">
        <!-- Tick range is now set by min/max price inputs above -->
        <div class="input-group">
          <label for="duration">Duration:</label>
          <select id="duration">
            <option value="0"""" <> (if state.selectedParams.duration == 0 then " selected" else "") <> """>Spot (0 days)</option>
            <option value="30"""" <> (if state.selectedParams.duration == 30 then " selected" else "") <> """>30 days</option>
            <option value="60"""" <> (if state.selectedParams.duration == 60 then " selected" else "") <> """>60 days</option>
            <option value="90"""" <> (if state.selectedParams.duration == 90 then " selected" else "") <> """>90 days</option>
          </select>
        </div>
        <div class="input-group">
          <label for="leverageRate">Leverage:</label>
          <input type="number" id="leverageRate" value="""" <> show state.selectedParams.leverageRate <> """" min="0" max="2.0" step="0.1">
          <small>0 = no leverage</small>
        </div>
      </div>
    </div>
    
    <div class="position-preview">
      <h4>Position Preview</h4>
      <div class="preview-info">
        <div>Type: """ <> describePosition state.selectedParams <> """</div>
        <div>Risk Profile: """ <> formatRiskAssessment (getRiskAssessment state.selectedParams) <> """</div>
        <div>Estimated Fee: """ <> formatFeePercentage (calculatePositionFee state.selectedParams defaultSystemParams) <> """</div>
      </div>
    </div>
    
    <button class="btn execute-btn" id="executeBtn">
      """ <> (case state.actionType of
        "create" -> "Create Position"
        "change" -> "Modify Position"
        "exit" -> "Exit Position"
        "createToken" -> "Create Token"
        _ -> "Execute") <> """
    </button>
  </div>""" <> 
  (if state.actionType == "createToken" then generateTokenCreationUI state else "") <> """
</div>
"""

-- Helper to render token balance
renderTokenBalance :: { ticker :: String, amount :: Number } -> String
renderTokenBalance token = """
      <div class="balance-item">
        <span>""" <> token.ticker <> """:</span>
        <strong>""" <> show token.amount <> """</strong>
      </div>
"""

-- Generate token options for select
generateTokenOptions :: AppState -> String
generateTokenOptions state = 
  let defaultOption = "<option value=\"DEFAULT\"" <> 
                     (case state.selectedPair of
                       Token "DEFAULT" -> " selected"
                       _ -> "") <> ">DEFAULT</option>"
      tokenOptions = fold (mapArray (\token -> 
        "<option value=\"" <> token.ticker <> "\"" <>
        (case state.selectedPair of
          Token t -> if t == token.ticker then " selected" else ""
          _ -> "") <> ">" <> token.ticker <> 
        (if token.launched then "" else " (Not Launched)") <> "</option>"
      ) state.tokens)
  in defaultOption <> tokenOptions

-- Generate token creation UI
generateTokenCreationUI :: AppState -> String
generateTokenCreationUI state = """
  <div class="token-creation">
    <h3>Create New Token</h3>
    <div class="input-group">
      <label for="tokenTicker">Ticker (3-10 chars):</label>
      <input type="text" id="tokenTicker" value="""" <> state.tokenCreation.ticker <> """" maxlength="10">
    </div>
    <div class="input-group">
      <label for="tokenName">Token Name:</label>
      <input type="text" id="tokenName" value="""" <> state.tokenCreation.name <> """">
    </div>
    <div class="info-box">
      <p>Note: Token will not be tradeable until 100 SOL is deposited to launch it.</p>
    </div>
  </div>
"""

-- Generate HTML for the application  
generateHTML :: AppState -> String
generateHTML state = """
<div class="container">
  <h1>Feels Protocol - Tick Position Orderbook</h1>
  
  <div class="main-layout">
    """ <> generateControlPanel state <> """
    """ <> generateOrderbook state <> """
  </div>
</div>
"""

-- Attach handlers after render
attachHandlersAfterRender :: AppState -> Effect Unit
attachHandlersAfterRender state = do
  -- Store current state globally
  setGlobalState state
  -- Attach handlers after a delay
  setTimeout attachHandlers 50

-- Execute position creation
executeCreatePosition :: AppState -> Effect Unit
executeCreatePosition state = do
  log "Creating new position..."
  
  -- Validate inputs
  if state.inputAmount <= 0.0 then do
    log "Error: Invalid amount"
    pure unit
  else if state.inputAmount > getBalance state state.selectedToken then do
    log "Error: Insufficient balance"
    pure unit
  else do
    -- Create the position
    case state.pool of
      Nothing -> do
        -- Initialize pool first
        log "Initializing new pool..."
        pool <- initPool SyntheticSOL state.selectedPair state.currentPrice
        result <- addLiquidity pool "user" state.inputAmount 0.0 
                    (priceToTick state.priceRange.min) 
                    (priceToTick state.priceRange.max) 
                    state.selectedParams 
                    defaultSystemParams
        
        -- Update state with new pool and position
        let newBalance = updateBalance state.balance state.selectedToken (0.0 - state.inputAmount)
            
        let newState = state { pool = Just result.pool, balance = newBalance }
        setGlobalState newState
        renderUI newState
        log $ "Position created with ID: " <> show result.position.id
      
      Just pool -> do
        -- Add to existing pool
        result <- addLiquidity pool "user" state.inputAmount 0.0 
                    (priceToTick state.priceRange.min) 
                    (priceToTick state.priceRange.max) 
                    state.selectedParams 
                    defaultSystemParams
        
        -- Update balance
        let newBalance = updateBalance state.balance state.selectedToken (0.0 - state.inputAmount)
            
        let newState = state { pool = Just result.pool, balance = newBalance }
        setGlobalState newState
        renderUI newState
        log $ "Position created with ID: " <> show result.position.id

-- Execute position modification
executeModifyPosition :: AppState -> Effect Unit
executeModifyPosition state = do
  log "Modifying position not yet implemented"
  -- TODO: Implement position modification logic

-- Execute position exit
executeExitPosition :: AppState -> Effect Unit
executeExitPosition state = do
  log "Exiting position not yet implemented"
  -- TODO: Implement position exit logic

-- Attach all event handlers
attachHandlers :: Effect Unit
attachHandlers = do
  win <- window
  doc <- document win
  let parentNode = toNonElementParentNode doc
  
  -- Pair selector
  maybePairSelect <- getElementById "pairSelect" parentNode
  case maybePairSelect of
    Just select -> do
      removeAllEventListeners select "change"
      addEventListener select "change" $ do
        currentState <- getGlobalState
        value <- getValue select
        let newPair = Token value  -- Value is the ticker from the select
            newState = currentState { selectedPair = newPair }
        -- Initialize pool if needed
        case currentState.pool of
          Nothing -> do
            pool <- initPool SyntheticSOL newPair 1.0
            let stateWithPool = newState { pool = Just pool }
            setGlobalState stateWithPool
            renderUI stateWithPool
          Just _ -> do
            setGlobalState newState
            renderUI newState
    Nothing -> pure unit
  
  -- Amount input
  maybeAmount <- getElementById "amount" parentNode
  case maybeAmount of
    Just input -> do
      removeAllEventListeners input "input"
      addEventListener input "input" $ do
        currentState <- getGlobalState
        value <- getValue input
        let amount = parseFloat value
            newState = currentState { inputAmount = amount }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit
  
  -- Price range inputs
  maybeMinPrice <- getElementById "minPrice" parentNode
  case maybeMinPrice of
    Just input -> do
      removeAllEventListeners input "input"
      addEventListener input "input" $ do
        currentState <- getGlobalState
        value <- getValue input
        let minPrice = parseFloat value
            newParams = currentState.selectedParams { tickLower = priceToTick minPrice }
            newState = currentState { priceRange = currentState.priceRange { min = minPrice }
                                    , selectedParams = newParams }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit
    
  maybeMaxPrice <- getElementById "maxPrice" parentNode
  case maybeMaxPrice of
    Just input -> do
      removeAllEventListeners input "input"
      addEventListener input "input" $ do
        currentState <- getGlobalState
        value <- getValue input
        let maxPrice = parseFloat value
            newParams = currentState.selectedParams { tickUpper = priceToTick maxPrice }
            newState = currentState { priceRange = currentState.priceRange { max = maxPrice }
                                    , selectedParams = newParams }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit
  
  -- Unified parameter inputs
  attachParamHandlers
  
  -- Execute button
  maybeExecuteBtn <- getElementById "executeBtn" parentNode
  case maybeExecuteBtn of
    Just btn -> do
      removeAllEventListeners btn "click"
      addEventListener btn "click" $ do
        currentState <- getGlobalState
        case currentState.actionType of
          "create" -> executeCreatePosition currentState
          "change" -> executeModifyPosition currentState
          "exit" -> executeExitPosition currentState
          "createToken" -> executeCreateToken currentState
          _ -> log "Unknown action type"
    Nothing -> pure unit
  
  -- Action buttons
  attachActionButtons

-- Attach action button handlers
attachActionButtons :: Effect Unit
attachActionButtons = do
  win <- window
  doc <- document win
  let parentNode = toNonElementParentNode doc
  
  -- Get all action buttons and attach handlers
  let actions = ["create", "change", "exit"]
  let attachActionHandler action = do
        let buttonQuery = "[data-action=\"" <> action <> "\"]"
        maybeBtn <- querySelector (QuerySelector buttonQuery) (toParentNode doc)
        case maybeBtn of
          Just btn -> do
            addEventListener btn "click" $ do
              currentState <- getGlobalState
              let newState = currentState { actionType = action }
              setGlobalState newState
              renderUI newState
          Nothing -> pure unit
  
  -- Attach handlers for each action
  sequence_ (map attachActionHandler actions)

-- Attach parameter handlers
attachParamHandlers :: Effect Unit
attachParamHandlers = do
  win <- window
  doc <- document win
  let parentNode = toNonElementParentNode doc
  
  -- Tick range is now handled by price range inputs
  
  -- Duration
  maybeDuration <- getElementById "duration" parentNode
  case maybeDuration of
    Just select -> do
      removeAllEventListeners select "change"
      addEventListener select "change" $ do
        currentState <- getGlobalState
        value <- getValue select
        let duration = case value of
              "0" -> 0
              "30" -> 30
              "60" -> 60
              "90" -> 90
              _ -> 0
            newParams = currentState.selectedParams { duration = duration }
            newState = currentState { selectedParams = newParams }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit
  
  -- Leverage rate
  maybeLeverageRate <- getElementById "leverageRate" parentNode
  case maybeLeverageRate of
    Just input -> do
      removeAllEventListeners input "input"
      addEventListener input "input" $ do
        currentState <- getGlobalState
        value <- getValue input
        let leverageRate = parseFloat value
            newParams = currentState.selectedParams { leverageRate = leverageRate }
            newState = currentState { selectedParams = newParams }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit
  
  -- Token creation inputs
  maybeTokenTicker <- getElementById "tokenTicker" parentNode
  case maybeTokenTicker of
    Just input -> do
      removeAllEventListeners input "input"
      addEventListener input "input" $ do
        currentState <- getGlobalState
        value <- getValue input
        let newState = currentState { tokenCreation = currentState.tokenCreation { ticker = value } }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit
    
  maybeTokenName <- getElementById "tokenName" parentNode
  case maybeTokenName of
    Just input -> do
      removeAllEventListeners input "input"
      addEventListener input "input" $ do
        currentState <- getGlobalState
        value <- getValue input
        let newState = currentState { tokenCreation = currentState.tokenCreation { name = value } }
        setGlobalState newState
        renderUI newState
    Nothing -> pure unit

-- Render the UI
renderUI :: AppState -> Effect Unit
renderUI state = do
  win <- window
  doc <- document win
  maybeRoot <- querySelector (QuerySelector "#app") (toParentNode doc)
  
  case maybeRoot of
    Nothing -> log "Could not find #app element"
    Just root -> do
      let html = generateHTML state
      setInnerHTML root html
      -- Attach handlers after render
      attachHandlersAfterRender state

-- Note: Array helper functions are now imported from Utils module
appendArray :: forall a. Array a -> Array a -> Array a
appendArray xs ys = xs <> ys

-- Update balance helper
updateBalance :: { sol :: Number, tokens :: Array { ticker :: String, amount :: Number } } -> TokenType -> Number -> { sol :: Number, tokens :: Array { ticker :: String, amount :: Number } }
updateBalance balance tokenType delta = case tokenType of
  SyntheticSOL -> balance { sol = balance.sol + delta }
  Token ticker -> 
    let updateToken t = if t.ticker == ticker 
                        then t { amount = t.amount + delta }
                        else t
    in balance { tokens = mapArray updateToken balance.tokens }
  _ -> balance

-- Execute token creation
executeCreateToken :: AppState -> Effect Unit
executeCreateToken state = do
  if isValidTicker state.tokenCreation.ticker && state.tokenCreation.name /= ""
  then do
    let params = { ticker: state.tokenCreation.ticker
                 , name: state.tokenCreation.name
                 , creator: "user"
                 }
    newToken <- createToken params
    let newState = state { tokens = appendArray state.tokens [newToken]
                         , tokenCreation = { ticker: "", name: "" }
                         , actionType = "create"
                         }
    setGlobalState newState
    renderUI newState
    log $ "Token created: " <> newToken.ticker
  else
    log "Invalid token parameters"

-- Execute token launch
executeLaunchToken :: AppState -> String -> Number -> Effect Unit
executeLaunchToken state ticker amount = do
  case find (\t -> t.ticker == ticker) state.tokens of
    Just token -> 
      if amount >= 100.0 && state.balance.sol >= amount
      then do
        let launchedToken = launchToken token amount
            updateToken t = if t.ticker == ticker then launchedToken else t
            newBalance = updateBalance state.balance SyntheticSOL (0.0 - amount)
            newState = state { tokens = mapArray updateToken state.tokens
                             , balance = newBalance
                             }
        setGlobalState newState
        renderUI newState
        log $ "Token launched: " <> ticker
      else
        log "Insufficient SOL or amount less than 100"
    Nothing -> log "Token not found"
  where
    find :: forall a. (a -> Boolean) -> Array a -> Maybe a
    find pred arr = case filter pred arr of
      [x] -> Just x
      _ -> Nothing