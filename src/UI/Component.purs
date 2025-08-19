-- UI Components for the Feels Protocol application
-- Contains render functions for different panels and UI elements
module UI.Component
  ( renderSystemPanel
  , renderWalletPanel
  , renderTokenCreatorPanel
  , renderExchangePanel
  , renderLoanBookPanel
  , renderSimulationPanel
  ) where

import Prelude
import Data.Array (length, null, chunksOf, reverse)
import Data.String.Common (trim, joinWith)
import Data.String as String
import Data.String (Pattern(..), split, take, drop)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Number as Number
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..), fromRight)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Foreign (Foreign)
import UI.Util.Codecs (safeTokenFromForeign, safePositionFromForeign, safeLenderOfferFromForeign)

-- Import UI state and actions from State module
import UI.State (UIState, Action(..))
import UI.Component.FormElements (renderTextInput, renderNumberInput, renderButton, renderFormGroup, renderSelect, defaultTextInputConfig, defaultNumberInputConfig, defaultButtonConfig, defaultSelectConfig, ButtonStyle(..), createAssetSelectOptions, createTermTypeOptions, createLeverageOptions, createMarketScenarioOptions)
import UI.Component.DataDisplay (renderBalance, renderCompactBalance, renderMetric, renderPrimaryMetric, renderResultMetric, renderSupply, formatLargeNumber, formatCompactNumber, formatIntWithCommas)
import UI.Component.Panel (renderPanel, renderSection, defaultPanelConfig, defaultSectionConfig, PanelStyle(..))
import UI.Util.Validation (renderValidationWarnings)
import Protocol.Common (Position)
import Simulation.Analysis (SimulationResults)
import Simulation.Scenario (MarketScenario(..))
import Utils (formatAmount)

--------------------------------------------------------------------------------
-- System Panel Components
--------------------------------------------------------------------------------

-- System metrics panel
renderSystemPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderSystemPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel system-metrics-panel") ]
    [ HH.h2_ [ HH.text "System Metrics" ]
    , case state.protocolStats of
        Nothing -> HH.div 
          [ HP.class_ (HH.ClassName "loading-state") ]
          [ HH.text "Loading system metrics..." ]
        Just stats -> HH.div_
          [ -- Primary metrics (larger display)
            HH.div
              [ HP.class_ (HH.ClassName "primary-metrics") ]
              [ renderPrimaryMetric "Total Value Locked" 
                  (formatLargeNumber stats.totalValueLocked) 
                  "FeelsSOL"
              , renderPrimaryMetric "Active Positions" 
                  (formatIntWithCommas stats.activePositions) 
                  ""
              ]
          , -- Secondary metrics grid
            HH.div
              [ HP.class_ (HH.ClassName "metrics-grid") ]
              [ renderMetric "Total Users" (formatIntWithCommas stats.totalUsers)
              , renderMetric "Live Tokens" (formatIntWithCommas stats.liveTokens)
              , renderMetric "Lender Offers" (formatIntWithCommas stats.totalLenderOffers)
              , renderMetric "POL Balance" (formatCompactNumber stats.polBalance <> " FeelsSOL")
              ]
          , -- Token supplies section
            HH.div
              [ HP.class_ (HH.ClassName "supplies-section") ]
              [ HH.h4_ [ HH.text "Token Supplies" ]
              , HH.div
                  [ HP.class_ (HH.ClassName "supplies-grid") ]
                  [ renderSupply "FeelsSOL Supply" stats.feelsSOLSupply
                  , renderSupply "JitoSOL Locked" stats.jitoSOLLocked
                  ]
              ]
          ]
    ]

--------------------------------------------------------------------------------
-- Wallet Panel Components
--------------------------------------------------------------------------------

-- Unified wallet panel (balances, tokens, and positions)
renderWalletPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderWalletPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel wallet-panel") ]
    [ HH.h2_ [ HH.text "Your Wallet" ]
    -- Balances section
    , HH.div
        [ HP.class_ (HH.ClassName "wallet-section") ]
        [ HH.h3_ [ HH.text "Balances" ]
        , HH.div
            [ HP.class_ (HH.ClassName "balance-compact") ]
            [ renderCompactBalance "JitoSOL" state.jitoBalance
            , renderCompactBalance "FeelsSOL" state.feelsBalance
            ]
        ]
    -- Created tokens section
    , HH.div
        [ HP.class_ (HH.ClassName "wallet-section") ]
        [ HH.h3_ [ HH.text "Your Tokens" ]
        , if length state.userTokens == 0
          then HH.p
            [ HP.class_ (HH.ClassName "empty-state") ]
            [ HH.text "No tokens created yet" ]
          else HH.div
            [ HP.class_ (HH.ClassName "token-list") ]
            (map renderToken state.userTokens)
        ]
    -- Positions section
    , HH.div
        [ HP.class_ (HH.ClassName "wallet-section") ]
        [ HH.h3_ [ HH.text "Your Positions" ]
        , if length state.userPositions == 0
          then HH.p
            [ HP.class_ (HH.ClassName "empty-state") ]
            [ HH.text "No active positions" ]
          else HH.div
            [ HP.class_ (HH.ClassName "position-list") ]
            (map renderPosition state.userPositions)
        ]
    ]
  where
    renderToken foreignToken =
      let token = safeTokenFromForeign foreignToken
      in HH.div
        [ HP.class_ (HH.ClassName "list-item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "list-item__header") ]
            [ HH.strong_ [ HH.text token.ticker ]
            , HH.text $ " " <> token.name
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "token-status") ]
            [ if token.live
              then HH.span 
                [ HP.class_ (HH.ClassName "status-live") ] 
                [ HH.text "Live" ]
              else HH.span 
                [ HP.class_ (HH.ClassName "status-pending") ] 
                [ HH.text "Pending" ]
            ]
        ]
    
    renderPosition foreignPos =
      let pos = safePositionFromForeign foreignPos
      in HH.div
        [ HP.class_ (HH.ClassName "list-item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "list-item__header") ]
            [ HH.text $ "Position #" <> show pos.id
            , HH.span
                [ HP.class_ (HH.ClassName "status status-active") ]
                [ HH.text "Active" ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "list-item__content") ]
            [ HH.div_ [ HH.text $ "Initial: " <> formatAmount pos.amount <> " @ " <> formatAmount pos.price ]
            , HH.div_ [ HH.text $ "Current Value: " <> formatAmount pos.value ]
            , HH.div_ [ HH.text $ "Yield Earned: " <> formatAmount pos.accumulatedYield ]
            , HH.div_ [ HH.text $ "Leverage: " <> pos.leverage <> " | Duration: " <> pos.duration ]
            , HH.div_ [ HH.text $ "Shares: " <> formatAmount pos.shares ]
            ]
        ]
    

--------------------------------------------------------------------------------
-- Token Creation Panel Components
--------------------------------------------------------------------------------

-- Token creator panel (always visible)
renderTokenCreatorPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderTokenCreatorPanel state =
  renderPanel (defaultPanelConfig { title = Just "Create Feels Token" })
    [ HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ renderTextInput (defaultTextInputConfig
            { id = "create-token-ticker"
            , name = "create-token-ticker"
            , placeholder = "e.g., ALPHA"
            , value = state.tokenTicker
            , onChange = UpdateTokenTicker
            , label = Just "Token Ticker:"
            , required = true
            })
        , renderTextInput (defaultTextInputConfig
            { id = "create-token-name"
            , name = "create-token-name"
            , placeholder = "e.g., Alpha Protocol Token"
            , value = state.tokenName
            , onChange = UpdateTokenName
            , label = Just "Token Name:"
            , required = true
            })
        , renderValidationWarnings state.tokenValidationErrors
        , renderButton (defaultButtonConfig
            { text = "Create Token"
            , onClick = CreateTokenUI
            , style = Primary
            })
        ]
    , HH.p
        [ HP.class_ (HH.ClassName "info-text") ]
        [ HH.text "Tokens launch through the batch auction system" ]
    ]

--------------------------------------------------------------------------------
-- Exchange Panel Components
--------------------------------------------------------------------------------

-- Universal exchange panel
renderExchangePanel :: forall m. UIState -> H.ComponentHTML Action () m
renderExchangePanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Exchange" ]
    , -- Exchange form
      HH.div
        [ HP.class_ (HH.ClassName "exchange-form") ]
        [ -- From section
          HH.div
            [ HP.class_ (HH.ClassName "exchange-section") ]
            [ HH.h3_ [ HH.text "From" ]
            , HH.div
                [ HP.class_ (HH.ClassName "form-group") ]
                [ renderSelect (defaultSelectConfig
                    { id = "from-asset"
                    , name = "from-asset"
                    , options = createAssetSelectOptions
                    , selectedValue = state.selectedFromAsset
                    , onChange = SelectFromAsset
                    , label = Just "Asset:"
                    })
                ]
            , HH.div
                [ HP.class_ (HH.ClassName "form-group") ]
                [ renderNumberInput (defaultNumberInputConfig
                    { id = "from-amount"
                    , name = "from-amount"
                    , placeholder = "Enter amount"
                    , value = state.inputAmount
                    , onChange = UpdateInputAmount
                    , label = Just "Amount:"
                    , min = Just 0.0
                    , required = true
                    })
                ]
            ]
        , -- Exchange arrow
          HH.div
            [ HP.class_ (HH.ClassName "exchange-arrow") ]
            [ ]
        , -- To section
          HH.div
            [ HP.class_ (HH.ClassName "exchange-section") ]
            [ HH.h3_ [ HH.text "To" ]
            , HH.div
                [ HP.class_ (HH.ClassName "form-group") ]
                [ renderSelect (defaultSelectConfig
                    { id = "to-asset"
                    , name = "to-asset"
                    , options = createAssetSelectOptions
                    , selectedValue = state.selectedToAsset
                    , onChange = SelectToAsset
                    , label = Just "Asset/Position:"
                    })
                ]
            , -- Show position options if converting to position
              renderPositionOptions state
            ]
        , -- Route preview
          HH.div
            [ HP.class_ (HH.ClassName "route-preview") ]
            [ HH.h4_ [ HH.text "Route Preview" ]
            , HH.p
                [ HP.class_ (HH.ClassName "route-text") ]
                [ HH.text $ generateRouteText state ]
            ]
        , -- Exchange button
          HH.button
            [ HE.onClick \_ -> ExecuteExchange
            , HP.class_ (HH.ClassName "btn btn--primary btn--large")
            ]
            [ HH.text "Execute Exchange" ]
        ]
    ]
  where
    -- Show position-specific options when converting to a position
    renderPositionOptions state' =
      if String.contains (String.Pattern "position") (state'.selectedToAsset)
        then renderLendOptions state'
        else HH.div_ []
    
    -- Generate human-readable route description
    generateRouteText state' =
      let from = case state'.selectedFromAsset of
            "jitosol" -> "JitoSOL"
            "feelssol" -> "FeelsSOL"
            "position" -> "Position"
            _ -> "Asset"
          to = case state'.selectedToAsset of
            "position-spot" -> "Spot Position"
            "position-term" -> "Term Position"
            "jitosol" -> "JitoSOL"
            "feelssol" -> "FeelsSOL"
            _ -> "Asset"
      in from <> " → " <> to <> " (Atomic transaction)"
    

-- Lending options component (shared between position creation panels)
renderLendOptions :: forall m. UIState -> H.ComponentHTML Action () m
renderLendOptions state =
  HH.div_
    [ -- Term selection
      HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ renderSelect (defaultSelectConfig
            { id = "term-type"
            , name = "term-type"
            , options = createTermTypeOptions
            , selectedValue = state.selectedTermType
            , onChange = SetTermType
            , label = Just "Term Type:"
            })
        ]
    , -- Leverage tier
      HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ renderSelect (defaultSelectConfig
            { id = "leverage-tier"
            , name = "leverage-tier"
            , options = createLeverageOptions
            , selectedValue = state.selectedLeverage
            , onChange = SetLeverageType
            , label = Just "Risk Level:"
            })
        ]
    -- Duration info
    , renderDurationInfo state
    -- APY display
    , renderAPY state
    ]
  where
    renderDurationInfo state' =
      HH.div
        [ HP.class_ (HH.ClassName "duration-info") ]
        [ case state'.selectedTermType of
            "monthly" -> 
              let termInfo = calculateTermInfo state'
              in HH.div_
                [ HH.p_ 
                    [ HH.text "Monthly Term: Join the current 28-day cycle. Your position will mature with all other monthly positions at the end of this term." ]
                , HH.p
                    [ HP.class_ (HH.ClassName "term-status") ]
                    [ HH.text termInfo ]
                ]
            _ -> HH.p_ 
              [ HH.text "Spot: No term commitment. Withdraw anytime with immediate liquidity. Lower yields than term positions." ]
        ]
    
    -- Calculate current term information
    calculateTermInfo state' =
      case state'.api of
        Nothing -> "Term information unavailable"
        Just _ -> 
          -- Terms are 28 days (assuming ~5 blocks/minute = 201,600 blocks)
          -- This is simplified - in production would use actual term schedule
          let blocksPerTerm = 201600
              currentBlock = state'.currentBlock
              currentTermNumber = Int.floor (Int.toNumber currentBlock / Int.toNumber blocksPerTerm) + 1
              blocksIntoTerm = currentBlock `mod` blocksPerTerm
              blocksRemaining = blocksPerTerm - blocksIntoTerm
              daysRemaining = Int.floor (Int.toNumber blocksRemaining / 7200.0)  -- ~7200 blocks per day
          in "Current term #" <> show currentTermNumber <> " - " <> show daysRemaining <> " days remaining"
    
    renderAPY state' =
      let duration = case state'.selectedTermType of
            "monthly" -> "Monthly Term"
            _ -> "Spot"
          leverage = case state'.selectedLeverage of
            "junior" -> "Junior (3x)"
            _ -> "Senior (1x)"
          baseAPY = 5.0
          leverageMultiplier = if state'.selectedLeverage == "junior" then 3.0 else 1.0
          durationMultiplier = if state'.selectedTermType == "monthly" then 1.2 else 1.0
          totalAPY = baseAPY * leverageMultiplier * durationMultiplier
      in
        HH.div_
          [ HH.div 
              [ HP.class_ (HH.ClassName "apy-row") ]
              [ HH.span_ [ HH.text $ "Duration: " <> duration ]
              , HH.span_ [ HH.text $ "Leverage: " <> leverage ]
              ]
          , HH.div 
              [ HP.class_ (HH.ClassName "apy-total") ]
              [ HH.text $ show totalAPY <> "% APY" ]
          , HH.div
              [ HP.class_ (HH.ClassName "apy-breakdown") ]
              [ HH.text "(Base 5% × Leverage × Duration bonus)" ]
          ]

--------------------------------------------------------------------------------
-- Lending Book Panel Components
--------------------------------------------------------------------------------

-- Loan book panel (shows available positions/offers)
renderLoanBookPanel :: forall m. Array Position -> H.ComponentHTML Action () m
renderLoanBookPanel offers =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Available Offers" ]
    , if length offers == 0
      then HH.p_ [ HH.text "No lending offers available" ]
      else HH.div
        [ HP.class_ (HH.ClassName "offer-list") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "offer__header") ]
            [ HH.div_ [ HH.text "ID" ]
            , HH.div_ [ HH.text "Lender" ]
            , HH.div_ [ HH.text "Asset" ]
            , HH.div_ [ HH.text "Amount (Avail/Total)" ]
            , HH.div_ [ HH.text "Collateral Required" ]
            , HH.div_ [ HH.text "Terms" ]
            , HH.div_ [ HH.text "Status" ]
            ]
        , HH.div_ (map renderOffer offers)
        ]
    ]
  where
    renderOffer foreignOffer =
      let offer = safeLenderOfferFromForeign foreignOffer
      in
      HH.div
        [ HP.class_ (HH.ClassName "offer__item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "offer__id") ]
            [ HH.text $ "#" <> show offer.id ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__owner") ]
            [ HH.text $ formatAddress (show offer.owner) ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__asset") ]
            [ HH.text $ offer.leverage ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__amount") ]
            [ HH.text $ formatAmount offer.amount <> " shares" ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__collateral") ]
            [ HH.text $ "Locked: " <> formatAmount offer.lockedAmount ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__terms") ]
            [ HH.text "Spot" ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__status") ]
            [ renderOfferStatus offer ]
        ]


    formatAddress addr = 
      if String.length addr > 12
        then String.take 6 addr <> "..." <> String.drop (String.length addr - 4) addr
        else addr
    
    
    
    _formatCollateral offer = 
      -- Show the locked amount for position
      formatAmount offer.lockedAmount
    
    renderOfferStatus offer =
      let available = offer.amount - offer.lockedAmount
          total = offer.amount
          percentFilled = if total > 0.0 
                           then (total - available) / total * 100.0
                           else 0.0
      in if available <= 0.0
           then HH.span 
                  [ HP.class_ (HH.ClassName "status-filled") ] 
                  [ HH.text "Filled" ]
           else if percentFilled > 0.0
                  then HH.span 
                         [ HP.class_ (HH.ClassName "status-partial") ] 
                         [ HH.text $ show (Int.floor percentFilled) <> "% Filled" ]
                  else HH.span 
                         [ HP.class_ (HH.ClassName "status-available") ] 
                         [ HH.text "Available" ]

--------------------------------------------------------------------------------
-- Simulation Panel Components
--------------------------------------------------------------------------------

-- Simulation control panel
renderSimulationPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderSimulationPanel state =
  renderPanel (defaultPanelConfig { title = Just "Market Simulation" })
    [ HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ renderNumberInput (defaultNumberInputConfig
            { id = "simulation-blocks"
            , name = "simulation-blocks"
            , placeholder = "10-1000"
            , value = Int.toNumber state.simulationConfig.simulationBlocks
            , onChange = \v -> UpdateSimulationConfig \config -> 
                config { simulationBlocks = fromMaybe config.simulationBlocks (Int.fromString (show (Int.round v))) }
            , label = Just "Simulation Blocks:"
            , min = Just 10.0
            , max = Just 1000.0
            , step = Just 1.0
            , required = true
            })
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ renderNumberInput (defaultNumberInputConfig
            { id = "num-accounts"
            , name = "num-accounts"  
            , placeholder = "3-100"
            , value = Int.toNumber state.simulationConfig.numAccounts
            , onChange = \v -> UpdateSimulationConfig \config -> 
                config { numAccounts = fromMaybe config.numAccounts (Int.fromString (show (Int.round v))) }
            , label = Just "Number of Accounts:"
            , min = Just 3.0
            , max = Just 100.0
            , step = Just 1.0
            , required = true
            })
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ renderSelect (defaultSelectConfig
            { id = "market-scenario"
            , name = "market-scenario"
            , options = createMarketScenarioOptions
            , selectedValue = show state.simulationConfig.scenario
            , onChange = \v -> UpdateSimulationConfig \config -> 
                config { scenario = parseMarketScenario v }
            , label = Just "Market Scenario:"
            })
        ]
    , if state.simulationRunning
      then HH.div
        [ HP.class_ (HH.ClassName "simulation-status") ]
        [ HH.text "Running simulation..." ]
      else HH.button
        [ HE.onClick \_ -> RunSimulation
        , HP.class_ (HH.ClassName "btn btn--primary btn--large")
        , HP.id "run-simulation-btn"  -- This ID is required for WebSocket integration
        ]
        [ HH.text "Run Market Simulation" ]
    , case state.simulationResults of
        Nothing -> HH.text ""
        Just results -> renderSimulationResults results
    ]
  where
    parseMarketScenario "BullMarket" = BullMarket
    parseMarketScenario "BearMarket" = BearMarket
    parseMarketScenario "SidewaysMarket" = SidewaysMarket
    parseMarketScenario "VolatileMarket" = VolatileMarket
    parseMarketScenario _ = state.simulationConfig.scenario
    
renderSimulationResults :: forall m. SimulationResults -> H.ComponentHTML Action () m
renderSimulationResults results =
  HH.div
    [ HP.class_ (HH.ClassName "simulation-results") ]
    [ HH.h3_ [ HH.text "Simulation Results" ]
    , HH.div
        [ HP.class_ (HH.ClassName "results-grid") ]
        [ renderResultMetric "Total Users" (show results.totalUsers)
        , renderResultMetric "Active Positions" (show results.activePositions)
        , renderResultMetric "Total Volume" (formatAmount results.totalVolume)
        , renderResultMetric "Price Change" (formatPercentage results.priceChange)
        ]
    ]
  where
    formatPercentage :: Number -> String
    formatPercentage change = 
      let percent = change * 100.0
          sign = if percent >= 0.0 then "+" else ""
          rounded = Int.round percent
      in sign <> show rounded <> "%"

--------------------------------------------------------------------------------
-- Price Chart Panel Components
--------------------------------------------------------------------------------

