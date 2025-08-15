-- UI Components for the Feels Protocol application
-- Contains all render functions for different panels and UI elements
module UI.Component
  ( renderSystemPanel
  , renderWalletPanel
  , renderTokenCreatorPanel
  , renderExchangePanel
  , renderLoanBookPanel
  , renderSimulationPanel
  ) where

import Prelude
import Data.Array (length, null)
import Data.String.Common (trim)
import Data.String as String
import Data.String (contains, Pattern(..), split, joinWith, take, drop)
import Data.Number as Number
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Coerce (unsafeCoerce)

-- Import UI state and actions from State module
import UI.State (UIState, Action(..), parseTokenType)
import Protocol.Common (TokenMetadata, Position)
import Protocol.Position (Duration(..))
import Simulation.Analysis (SimulationResults)
import Simulation.Market (MarketScenario(..))
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
                  [ renderSupply "FeelsSOL Supply" (formatLargeNumber stats.feelsSOLSupply)
                  , renderSupply "JitoSOL Locked" (formatLargeNumber stats.jitoSOLLocked)
                  ]
              ]
          ]
    ]
  where
    renderPrimaryMetric label value unit =
      HH.div
        [ HP.class_ (HH.ClassName "primary-metric") ]
        [ HH.div [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text label ]
        , HH.div 
            [ HP.class_ (HH.ClassName "metric-value-large") ] 
            [ HH.span [ HP.class_ (HH.ClassName "value") ] [ HH.text value ]
            , if unit /= "" 
              then HH.span [ HP.class_ (HH.ClassName "unit") ] [ HH.text $ " " <> unit ]
              else HH.text ""
            ]
        ]
    
    renderMetric label value =
      HH.div
        [ HP.class_ (HH.ClassName "metric-item") ]
        [ HH.div [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "metric-value") ] [ HH.text value ]
        ]
    
    renderSupply label value =
      HH.div
        [ HP.class_ (HH.ClassName "supply-item") ]
        [ HH.span [ HP.class_ (HH.ClassName "supply-label") ] [ HH.text $ label <> ":" ]
        , HH.span [ HP.class_ (HH.ClassName "supply-value") ] [ HH.text value ]
        ]
    
    -- Format large numbers with comma separators
    formatLargeNumber :: Number -> String
    formatLargeNumber n = 
      let str = formatAmount n
          parts = split (Pattern ".") str
      in case parts of
           [whole, decimal] -> addCommas whole <> "." <> decimal
           [whole] -> addCommas whole
           _ -> str
    
    -- Format compact numbers (K/M notation)
    formatCompactNumber :: Number -> String
    formatCompactNumber n = 
      if n >= 1000000.0 
      then formatAmount (n / 1000000.0) <> "M"
      else if n >= 1000.0
      then formatAmount (n / 1000.0) <> "K"
      else formatAmount n
    
    -- Format integers with commas
    formatIntWithCommas :: Int -> String
    formatIntWithCommas n = addCommas (show n)
    
    -- Add commas to number strings
    addCommas :: String -> String
    addCommas s = 
      let len = String.length s
      in if len <= 3 
         then s
         else if len <= 6
         then take (len - 3) s <> "," <> drop (len - 3) s
         else if len <= 9
         then take (len - 6) s <> "," <> take 3 (drop (len - 6) s) <> "," <> drop (len - 3) s
         else take (len - 9) s <> "," <> take 3 (drop (len - 9) s) <> "," <> take 3 (drop (len - 6) s) <> "," <> drop (len - 3) s

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
            [ renderBalance "JitoSOL" state.jitoBalance
            , renderBalance "FeelsSOL" state.feelsBalance
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
    renderBalance label amount =
      HH.div
        [ HP.class_ (HH.ClassName "balance-line")
        , HP.style "margin: 0; padding: 2px 0; line-height: 1.2;"
        ]
        [ HH.span [ HP.class_ (HH.ClassName "balance-label") ] [ HH.text $ label <> ": " ]
        , HH.span [ HP.class_ (HH.ClassName "balance-value") ] [ HH.text $ formatAmount amount ]
        ]
    
    renderToken foreignToken =
      let token = unsafeCoerce foreignToken :: { ticker :: String, name :: String, live :: Boolean }
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
      let pos = unsafeCoerce foreignPos :: { id :: Int, amount :: Number, leverage :: String, price :: Number, duration :: String, shares :: Number, value :: Number, accumulatedYield :: Number }
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
            [ HH.div_ [ HH.text $ "Initial: " <> formatAmount pos.amount <> " @ " <> formatPrice pos.price ]
            , HH.div_ [ HH.text $ "Current Value: " <> formatAmount pos.value ]
            , HH.div_ [ HH.text $ "Yield Earned: " <> formatAmount pos.accumulatedYield ]
            , HH.div_ [ HH.text $ "Leverage: " <> pos.leverage <> " | Duration: " <> pos.duration ]
            , HH.div_ [ HH.text $ "Shares: " <> formatAmount pos.shares ]
            ]
        ]
    
    formatPrice p = formatAmount p

--------------------------------------------------------------------------------
-- Token Creation Panel Components
--------------------------------------------------------------------------------

-- Token creator panel (always visible)
renderTokenCreatorPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderTokenCreatorPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Create Feels Token" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "create-token-ticker" ] [ HH.text "Token Ticker:" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "create-token-ticker"
            , HP.name "create-token-ticker"
            , HP.placeholder "e.g., ALPHA"
            , HP.class_ (HH.ClassName "form__input")
            , HP.value state.tokenTicker
            , HE.onValueChange \v -> UpdateTokenTicker (trim v)
            ]
        , HH.label [ HP.for "create-token-name" ] [ HH.text "Token Name:" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "create-token-name"
            , HP.name "create-token-name"
            , HP.placeholder "e.g., Alpha Protocol Token"
            , HP.class_ (HH.ClassName "form__input")
            , HP.value state.tokenName
            , HE.onValueChange \v -> UpdateTokenName (trim v)
            ]
        , renderValidationWarnings state.tokenValidationErrors
        , HH.button
            [ HE.onClick \_ -> CreateTokenUI
            , HP.class_ (HH.ClassName "btn btn--primary")
            ]
            [ HH.text "Create Token" ]
        ]
    , HH.p
        [ HP.class_ (HH.ClassName "info-text") ]
        [ HH.text "Tokens launch through the batch auction system" ]
    ]
  where
    renderValidationWarnings :: forall n. Array String -> H.ComponentHTML Action () n
    renderValidationWarnings errors =
      if null errors
        then HH.text ""
        else HH.div
          [ HP.class_ (HH.ClassName "validation-warnings") ]
          (map renderWarning errors)
    
    renderWarning :: forall n. String -> H.ComponentHTML Action () n  
    renderWarning error =
      HH.div
        [ HP.class_ (HH.ClassName "warning-message") ]
        [ HH.text error ]

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
                [ HH.label [ HP.for "from-asset" ] [ HH.text "Asset:" ]
                , HH.select
                    [ HP.id "from-asset"
                    , HP.name "from-asset"
                    , HE.onValueChange \v -> SelectFromAsset v
                    , HP.class_ (HH.ClassName "form__select")
                    ]
                    [ HH.option [ HP.value "jitosol" ] [ HH.text "JitoSOL" ]
                    , HH.option [ HP.value "feelssol" ] [ HH.text "FeelsSOL" ]
                    , HH.option [ HP.value "position" ] [ HH.text "Existing Position" ]
                    -- TODO: Add user tokens
                    ]
                ]
            , HH.div
                [ HP.class_ (HH.ClassName "form-group") ]
                [ HH.label [ HP.for "from-amount" ] [ HH.text "Amount:" ]
                , HH.input
                    [ HP.type_ HP.InputNumber
                    , HP.id "from-amount"
                    , HP.name "from-amount"
                    , HP.value (formatAmount state.inputAmount)
                    , HE.onValueChange \v -> UpdateInputAmount (fromMaybe 0.0 (Number.fromString v))
                    , HP.class_ (HH.ClassName "form__input")
                    , HP.placeholder "Enter amount"
                    ]
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
                [ HH.label [ HP.for "to-asset" ] [ HH.text "Asset/Position:" ]
                , HH.select
                    [ HP.id "to-asset"
                    , HP.name "to-asset"
                    , HE.onValueChange \v -> SelectToAsset v
                    , HP.class_ (HH.ClassName "form__select")
                    ]
                    [ HH.option [ HP.value "position-spot" ] [ HH.text "Spot Position" ]
                    , HH.option [ HP.value "position-term" ] [ HH.text "Term Position" ]
                    , HH.option [ HP.value "jitosol" ] [ HH.text "JitoSOL" ]
                    , HH.option [ HP.value "feelssol" ] [ HH.text "FeelsSOL" ]
                    -- TODO: Add user tokens
                    ]
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
        [ HH.label [ HP.for "term-type" ] [ HH.text "Term Type:" ]
        , HH.select
            [ HP.id "term-type"
            , HP.name "term-type"
            , HE.onValueChange SetTermType
            , HP.class_ (HH.ClassName "form__select")
            ]
            [ HH.option [ HP.value "spot", HP.selected true ] [ HH.text "Spot (Flexible)" ]
            , HH.option [ HP.value "monthly" ] [ HH.text "Monthly Term (Join Current Cycle)" ]
            ]
        ]
    , -- Leverage tier
      HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "leverage-tier" ] [ HH.text "Risk Level:" ]
        , HH.select
            [ HP.id "leverage-tier"
            , HP.name "leverage-tier"
            , HE.onValueChange SetLeverageType
            , HP.class_ (HH.ClassName "form__select")
            ]
            [ HH.option [ HP.value "senior", HP.selected true ] [ HH.text "Senior (1x - Protected)" ]
            , HH.option [ HP.value "junior" ] [ HH.text "Junior (3x - Higher Risk/Reward)" ]
            ]
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
        Just api -> 
          -- Terms are 28 days (assuming ~5 blocks/minute = 201,600 blocks)
          -- This is simplified - in production would use actual term schedule
          let blocksPerTerm = 201600
              currentBlock = 1000  -- TODO: Get from protocol state
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
      let offer = unsafeCoerce foreignOffer :: { id :: Int, owner :: String, leverage :: String, amount :: Number, lockedAmount :: Number }
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

    formatPositionTerms position = 
      -- Since position is a foreign type, we need to handle this carefully
      "Spot" -- Simplified for now to avoid type errors

    formatAddress addr = 
      if String.length addr > 12
        then String.take 6 addr <> "..." <> String.drop (String.length addr - 4) addr
        else addr
    
    formatPrice p = 
      formatAmount p
    
    formatDuration d = 
      if d == 0 
      then "Spot" 
      else show (d / 40320) <> " months"
    
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
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Market Simulation" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "simulation-blocks" ] [ HH.text "Simulation Blocks:" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "simulation-blocks"
            , HP.name "simulation-blocks"
            , HP.value (show state.simulationConfig.simulationBlocks)
            , HE.onValueChange \v -> UpdateSimulationConfig \config -> 
                config { simulationBlocks = fromMaybe config.simulationBlocks (Int.fromString (String.trim v)) }
            , HP.class_ (HH.ClassName "form__input")
            , HP.min 10.0
            , HP.max 1000.0
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "num-accounts" ] [ HH.text "Number of Accounts:" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "num-accounts"
            , HP.name "num-accounts"
            , HP.value (show state.simulationConfig.numAccounts)
            , HE.onValueChange \v -> UpdateSimulationConfig \config -> 
                config { numAccounts = fromMaybe config.numAccounts (Int.fromString v) }
            , HP.class_ (HH.ClassName "form__input")
            , HP.min 3.0
            , HP.max 100.0
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "market-scenario" ] [ HH.text "Market Scenario:" ]
        , HH.select
            [ HP.id "market-scenario"
            , HP.name "market-scenario"
            , HE.onValueChange \v -> UpdateSimulationConfig \config -> 
                config { scenario = parseMarketScenario v }
            , HP.class_ (HH.ClassName "form__select")
            ]
            [ HH.option [ HP.value "BullMarket" ] [ HH.text "Bull Market" ]
            , HH.option [ HP.value "BearMarket" ] [ HH.text "Bear Market" ]
            , HH.option [ HP.value "SidewaysMarket" ] [ HH.text "Sideways Market" ]
            , HH.option [ HP.value "VolatileMarket" ] [ HH.text "Volatile Market" ]
            ]
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
    renderResultMetric label value =
      HH.div
        [ HP.class_ (HH.ClassName "result-metric") ]
        [ HH.div [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "metric-value") ] [ HH.text value ]
        ]
    
    formatPercentage :: Number -> String
    formatPercentage change = 
      let percent = change * 100.0
          sign = if percent >= 0.0 then "+" else ""
          rounded = Int.round percent
      in sign <> show rounded <> "%"

--------------------------------------------------------------------------------
-- Price Chart Panel Components
--------------------------------------------------------------------------------

