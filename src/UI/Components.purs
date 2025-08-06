-- UI Components for the Feels Protocol application
-- Contains all render functions for different panels and UI elements
module UI.Components
  ( renderSystemPanel
  , renderGatewayPanel
  , renderWalletPanel
  , renderTokenCreatorPanel
  , renderUserTokensPanel
  , renderCreatePositionPanel
  , renderLoanBookPanel
  , renderPositionsPanel
  , renderPriceChartPanel
  , renderSimulationPanel
  ) where

import Prelude
import Data.Array ((:), length, find, last, head, zip, range, reverse, filter, groupBy, take, null, sortBy, drop)
import Data.Functor (map)
import Data.String.Common (trim, joinWith)
import Data.String as String
import Data.Number as Number
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Import UI state and actions from State module
import UI.State (UIState, Action(..), parseTokenType)
import Token (TokenType(..), TokenMetadata)
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..), LendingSide(..), getAvailableAmount)
import Simulation.Sim (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..))
import Utils (formatAmount, formatPercentage)

--------------------------------------------------------------------------------
-- System Panel Components
--------------------------------------------------------------------------------

-- System metrics panel
renderSystemPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderSystemPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "System Metrics" ]
    , case state.protocolStats of
        Nothing -> HH.div_ [ HH.text "Loading metrics..." ]
        Just stats -> HH.div
          [ HP.class_ (HH.ClassName "form-fields") ]
          [ renderMetric "Total Value Locked" $ formatAmount stats.totalValueLocked
          , renderMetric "FeelsSOL Supply" $ formatAmount stats.feelsSOLSupply
          , renderMetric "JitoSOL Locked" $ formatAmount stats.jitoSOLLocked
          , renderMetric "POL Balance" $ formatAmount stats.polBalance
          , renderMetric "Active Users" $ show stats.totalUsers
          , renderMetric "Active Positions" $ show stats.activePositions
          , renderMetric "Lender Offers" $ show stats.totalLenderOffers
          , renderMetric "Live Tokens" $ show stats.liveTokens
          ]
    ]
  where
    renderMetric label value =
      HH.div
        [ HP.class_ (HH.ClassName "list-item__content") ]
        [ HH.div [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "value") ] [ HH.text value ]
        ]

--------------------------------------------------------------------------------
-- Gateway Panel Components
--------------------------------------------------------------------------------

-- Gateway panel (always visible)
renderGatewayPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderGatewayPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Gateway" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "jitosol-amount" ] [ HH.text "JitoSOL Amount (for entering):" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "jitosol-amount"
            , HP.name "jitosol-amount"
            , HP.value (formatAmount state.jitoSOLAmount)
            , HE.onValueChange \v -> UpdateJitoSOLAmount (fromMaybe 0.0 (Number.fromString v))
            , HP.class_ (HH.ClassName "form__input")
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "feelssol-amount" ] [ HH.text "FeelsSOL Amount (for exiting):" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "feelssol-amount"
            , HP.name "feelssol-amount"
            , HP.value (formatAmount state.feelsSOLAmount)
            , HE.onValueChange \v -> UpdateFeelsSOLAmount (fromMaybe 0.0 (Number.fromString v))
            , HP.class_ (HH.ClassName "form__input")
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "gateway-actions") ]
        [ HH.button
            [ HE.onClick \_ -> EnterGateway
            , HP.class_ (HH.ClassName "btn btn--primary")
            ]
            [ HH.text "JitoSOL →", HH.br_, HH.text "FeelsSOL" ]
        , HH.button
            [ HE.onClick \_ -> ExitGateway
            , HP.class_ (HH.ClassName "btn btn--primary")
            ]
            [ HH.text "FeelsSOL →", HH.br_, HH.text "JitoSOL" ]
        ]
    ]

--------------------------------------------------------------------------------
-- Wallet Panel Components
--------------------------------------------------------------------------------

-- Wallet panel
renderWalletPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderWalletPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Your Wallet" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-fields") ]
        [ renderBalance "JitoSOL" state.jitoSOLBalance
        , renderBalance "FeelsSOL" state.feelsSOLBalance
        ]
    ]
  where
    renderBalance label amount =
      HH.div
        [ HP.class_ (HH.ClassName "list-item__content") ]
        [ HH.div [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "value") ] [ HH.text $ formatAmount amount ]
        ]

--------------------------------------------------------------------------------
-- Token Creation Panel Components
--------------------------------------------------------------------------------

-- Token creator panel (always visible)
renderTokenCreatorPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderTokenCreatorPanel state =
  let 
    hasErrors = not (null state.tokenValidationErrors)
    buttonClass = if hasErrors || trim state.tokenTicker == "" || trim state.tokenName == ""
                 then "btn btn--primary btn--disabled"
                 else "btn btn--primary"
  in
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Create Feels Token" ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "token-ticker" ] [ HH.text "Token Ticker:" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "token-ticker"
            , HP.name "token-ticker"
            , HP.placeholder "e.g., ALPHA"
            , HP.class_ (HH.ClassName "form__input")
            , HP.value state.tokenTicker
            , HE.onValueChange UpdateTokenTicker
            ]
        , HH.label [ HP.for "token-name" ] [ HH.text "Token Name:" ]
        , HH.input
            [ HP.type_ HP.InputText
            , HP.id "token-name"
            , HP.name "token-name"
            , HP.placeholder "e.g., Alpha Protocol Token"
            , HP.class_ (HH.ClassName "form__input")
            , HP.value state.tokenName
            , HE.onValueChange UpdateTokenName
            ]
        , renderValidationWarnings state.tokenValidationErrors
        , HH.button
            [ HE.onClick \_ -> CreateTokenUI
            , HP.class_ (HH.ClassName buttonClass)
            , HP.disabled (hasErrors || trim state.tokenTicker == "" || trim state.tokenName == "")
            ]
            [ HH.text "Create Token" ]
        ]
    , HH.p
        [ HP.class_ (HH.ClassName "info-text") ]
        [ HH.text "Tokens launch when 100 FeelsSOL is staked" ]
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
-- User Tokens Panel Components
--------------------------------------------------------------------------------

-- User tokens panel
renderUserTokensPanel :: forall m. Array TokenMetadata -> H.ComponentHTML Action () m
renderUserTokensPanel tokens =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Your Created Tokens" ]
    , if length tokens == 0
      then HH.p_ [ HH.text "No tokens created yet" ]
      else HH.div
        [ HP.class_ (HH.ClassName "token-list") ]
        (map renderToken tokens)
    ]
  where
    renderToken token =
      HH.div
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
                [ HH.text $ formatAmount token.stakedFeelsSOL <> "/100 FeelsSOL" ]
            ]
        ]

--------------------------------------------------------------------------------
-- Position Creation Panel Components
--------------------------------------------------------------------------------

-- Create position panel
renderCreatePositionPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderCreatePositionPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Create Position" ]
    , -- Position form
      HH.div
        [ HP.class_ (HH.ClassName "position-form") ]
        [ -- Amount input
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "amount" ] [ HH.text "Amount:" ]
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.id "amount"
                , HP.name "amount"
                , HP.value (formatAmount state.inputAmount)
                , HE.onValueChange \v -> UpdateInputAmount (fromMaybe 0.0 (Number.fromString v))
                , HP.class_ (HH.ClassName "form__input")
                ]
            ]
        , -- Collateral selector (moved above asset)
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "collateral-asset" ] [ HH.text "Input Asset:" ]
            , HH.select
                [ HP.id "collateral-asset"
                , HP.name "collateral-asset"
                , HE.onValueChange \v -> SelectCollateralAsset (parseTokenType v)
                , HP.class_ (HH.ClassName "form__select")
                ]
                ([ HH.option [ HP.value "JitoSOL" ] [ HH.text "JitoSOL" ]
                 , HH.option [ HP.value "FeelsSOL" ] [ HH.text "FeelsSOL" ]
                 ] <> map (\token -> HH.option [ HP.value token.ticker ] [ HH.text token.ticker ]) state.userTokens)
            ]
        , -- Asset selector (moved below collateral)
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "output-asset" ] [ HH.text "Output Asset:" ]
            , HH.select
                [ HP.id "output-asset"
                , HP.name "output-asset"
                , HE.onValueChange \v -> SelectAsset (parseTokenType v)
                , HP.class_ (HH.ClassName "form__select")
                ]
                ([ HH.option [ HP.value "FeelsSOL" ] [ HH.text "FeelsSOL" ]
                 , HH.option [ HP.value "JitoSOL" ] [ HH.text "JitoSOL" ]
                 ] <> map (\token -> HH.option [ HP.value token.ticker ] [ HH.text token.ticker ]) state.userTokens)
            ]
        , -- Staking options
          renderLendOptions state
        , -- Leverage options
          renderLeverageOptions state
        , -- Create button
          HH.button
            [ HE.onClick \_ -> CreatePosition
            , HP.class_ (HH.ClassName "btn btn--primary btn--large")
            ]
            [ HH.text "Create Position" ]
        ]
    ]
  where
    renderLendOptions state' =
      HH.div_
        [ -- Unbonding period
          HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "unbonding-period" ] [ HH.text "Unbonding Period:" ]
            , HH.select
                [ HP.id "unbonding-period"
                , HP.name "unbonding-period"
                , HE.onValueChange \v -> SetUnbondingPeriod $ case v of
                    "infinite" -> Infinite
                    "30" -> Days30
                    "60" -> Days60
                    _ -> Days90
                , HP.class_ (HH.ClassName "form__select")
                ]
                [ HH.option [ HP.value "infinite", HP.selected true ] [ HH.text "Infinite (Swap)" ]
                , HH.option [ HP.value "30" ] [ HH.text "30 Days" ]
                , HH.option [ HP.value "60" ] [ HH.text "60 Days" ]
                , HH.option [ HP.value "90" ] [ HH.text "90 Days" ]
                ]
            ]
        ]

    renderLeverageOptions _ =
      HH.div
        [ HP.class_ (HH.ClassName "form-group") ]
        [ HH.label [ HP.for "leverage" ] [ HH.text "Leverage:" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.id "leverage"
            , HP.name "leverage"
            , HP.value (formatAmount state.leverage)
            , HE.onValueChange \v -> SetLeverage (fromMaybe 2.0 (Number.fromString v))
            , HP.min 1.0
            , HP.max 10.0
            , HP.attr (HH.AttrName "step") "0.5"
            , HP.class_ (HH.ClassName "form__input")
            ]
        ]

--------------------------------------------------------------------------------
-- Lending Book Panel Components
--------------------------------------------------------------------------------

-- Loan book panel
renderLoanBookPanel :: forall m. Array LendingRecord -> H.ComponentHTML Action () m
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
    renderOffer offer =
      HH.div
        [ HP.class_ (HH.ClassName "offer__item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "offer__id") ]
            [ HH.text $ "#" <> show offer.id ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__owner") ]
            [ HH.text $ formatAddress offer.owner ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__asset") ]
            [ HH.text $ show offer.lendAsset ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__amount") ]
            [ HH.text $ formatAmount (getAvailableAmount offer) <> " / " <> formatAmount offer.lendAmount ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__collateral") ]
            [ HH.text $ formatCollateral offer ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__terms") ]
            [ HH.text $ formatTerms offer.terms ]
        , HH.div
            [ HP.class_ (HH.ClassName "offer__status") ]
            [ renderOfferStatus offer ]
        ]

    formatTerms = case _ of
      SwapTerms -> "Swap"
      StakingTerms period -> "Staking (" <> show period <> ")"
      LeverageTerms lev -> "Leverage (" <> formatAmount lev <> "x)"

    formatRate = case _ of
      SwapTerms -> "Market"
      StakingTerms _ -> "5% APY"
      LeverageTerms _ -> "Variable"
    
    formatAddress addr = 
      if String.length addr > 12
        then String.take 6 addr <> "..." <> String.drop (String.length addr - 4) addr
        else addr
    
    formatCollateral offer = 
      show offer.collateralAsset <> " " <> 
      case offer.side of
        Lender -> formatAmount (offer.collateralAmount * 100.0) <> "%"  -- Show as ratio percentage
        _ -> formatAmount offer.collateralAmount
    
    formatAge createdAt = 
      -- For now, just show "New" - in a real app would calculate time difference
      "New"
    
    renderOfferStatus offer =
      let available = getAvailableAmount offer
          total = offer.lendAmount
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
-- Positions Panel Components
--------------------------------------------------------------------------------

-- Positions panel
renderPositionsPanel :: forall m. Array LendingRecord -> H.ComponentHTML Action () m
renderPositionsPanel positions =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Your Created Positions" ]
    , if length positions == 0
      then HH.p_ [ HH.text "No active positions" ]
      else HH.div
        [ HP.class_ (HH.ClassName "position-list") ]
        (map renderPosition positions)
    ]
  where
    renderPosition pos =
      HH.div
        [ HP.class_ (HH.ClassName "list-item") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "list-item__header") ]
            [ HH.text $ "Position #" <> show pos.id <> " (" <> show pos.side <> ")"
            , HH.span
                [ HP.class_ (HH.ClassName $ "status " <> statusClass pos.status) ]
                [ HH.text $ show pos.status ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "list-item__content") ]
            [ HH.div_ [ HH.text $ formatAmount pos.lendAmount <> " " <> show pos.lendAsset ]
            , HH.div_ [ HH.text $ "Collateral: " <> formatAmount pos.collateralAmount <> " " <> show pos.collateralAsset ]
            , HH.div_ [ HH.text $ "Terms: " <> formatTerms pos.terms ]
            ]
        ]
    
    formatTerms = case _ of
      SwapTerms -> "Swap"
      StakingTerms period -> "Staking (" <> show period <> ")"
      LeverageTerms lev -> "Leverage (" <> formatAmount lev <> "x)"

    statusClass status = case status of
      _ -> "status-active"  -- Simplified for now

--------------------------------------------------------------------------------
-- Price Chart Panel Components
--------------------------------------------------------------------------------

-- Helper function to format chart data points as JSON
formatChartPoint :: { timestamp :: Number, block :: Int, price :: Number, polValue :: Number, tokens :: Array { ticker :: String, price :: Number, polFloor :: Number, live :: Boolean } } -> String
formatChartPoint point = 
  "{\"timestamp\":" <> show point.timestamp <> 
  ",\"block\":" <> show point.block <> 
  ",\"price\":" <> show point.price <> 
  ",\"polValue\":" <> show point.polValue <> 
  ",\"tokens\":{" <> 
  (joinWith "," $ map (\t -> "\"" <> t.ticker <> "\":{\"ticker\":\"" <> t.ticker <> "\",\"price\":" <> show t.price <> ",\"polFloor\":" <> show t.polFloor <> ",\"live\":" <> show t.live <> "}") point.tokens) <>
  "}}"

-- Price chart panel
renderPriceChartPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderPriceChartPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Price History" ]
    , case length state.priceHistory of
        0 -> HH.div
               [ HP.class_ (HH.ClassName "no-price-data") ]
               [ HH.text "No price data available - run simulation to generate history" ]
        _ -> HH.div_
              [ -- Chart canvas
                HH.div
                  [ HP.class_ (HH.ClassName "chart-container") ]
                  [ HH.element (HH.ElemName "canvas")
                      [ HP.id "price-chart"
                      , HP.attr (HH.AttrName "width") "600"
                      , HP.attr (HH.AttrName "height") "300"
                      ]
                      []
                  ]
              -- Hidden chart data for JavaScript
              , HH.div
                  [ HP.class_ (HH.ClassName "chart-data")
                  , HP.style "display: none;"
                  ]
                  [ HH.text $ "[" <> (joinWith "," $ map formatChartPoint state.priceHistory) <> "]" ]
              ]
    ]

--------------------------------------------------------------------------------
-- Simulation Panel Components
--------------------------------------------------------------------------------

-- Simulation panel
renderSimulationPanel :: forall m. UIState -> H.ComponentHTML Action () m
renderSimulationPanel state =
  HH.div
    [ HP.class_ (HH.ClassName "panel") ]
    [ HH.h2_ [ HH.text "Market Simulation" ]
    , -- Simulation config
      HH.div
        [ HP.class_ (HH.ClassName "position-form") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "market-scenario" ] [ HH.text "Market Scenario:" ]
            , HH.select
                [ HP.id "market-scenario"
                , HP.name "market-scenario"
                , HE.onValueChange \v -> UpdateSimulationConfig $ _ { scenario = parseScenario v }
                , HP.class_ (HH.ClassName "form__select")
                ]
                [ HH.option [ HP.value "bull" ] [ HH.text "Bull Market" ]
                , HH.option [ HP.value "bear" ] [ HH.text "Bear Market" ]
                , HH.option [ HP.value "volatile" ] [ HH.text "High Volatility" ]
                , HH.option [ HP.value "stable" ] [ HH.text "Stable" ]
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "user-count" ] [ HH.text "User Count:" ]
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.id "user-count"
                , HP.name "user-count"
                , HP.value (show state.simulationConfig.numAccounts)
                , HE.onValueChange \v -> UpdateSimulationConfig $ 
                    _ { numAccounts = fromMaybe 5 (Int.fromString v) }
                , HP.min 1.0
                , HP.max 20.0  -- Reduced from 100
                , HP.class_ (HH.ClassName "form__input")
                ]
            ]
        , HH.div
            [ HP.class_ (HH.ClassName "form-group") ]
            [ HH.label [ HP.for "blocks" ] [ HH.text "Blocks to Simulate:" ]
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.id "blocks"
                , HP.name "blocks"
                , HP.value (show state.simulationConfig.simulationBlocks)
                , HE.onValueChange \v -> UpdateSimulationConfig $ 
                    _ { simulationBlocks = fromMaybe 100 (Int.fromString v) }
                , HP.min 5.0
                , HP.max 200.0  -- Allow up to 200 blocks
                , HP.class_ (HH.ClassName "form__input")
                ]
            ]
        ]
    , -- Run simulation button
      HH.button
        [ HE.onClick \_ -> RunSimulation
        , HP.class_ (HH.ClassName "btn btn--primary btn--large")
        , HP.id "run-simulation-btn"
        , HP.disabled state.simulationRunning
        ]
        [ HH.text $ if state.simulationRunning then "Running..." else "Run Simulation" ]
    , -- Results
      case state.simulationResults of
        Nothing -> HH.text ""
        Just results -> renderSimulationResults results
    ]
  where
    parseScenario = case _ of
      "bear" -> BearMarket
      "volatile" -> VolatileMarket  
      "stable" -> SidewaysMarket
      _ -> BullMarket

    renderSimulationResults results =
      HH.div
        [ HP.class_ (HH.ClassName "simulation-results") ]
        [ HH.h3_ [ HH.text "Simulation Results" ]
        , HH.div
            [ HP.class_ (HH.ClassName "results-grid") ]
            [ renderResult "Protocol TVL" $ formatAmount results.protocolTVL
            , renderResult "Total Volume" $ formatAmount results.totalVolume
            , renderResult "Price Change" $ formatPercentage results.priceChange
            , renderResult "Volatility" $ formatPercentage results.volatility
            , renderResult "Active Users" $ show results.totalUsers
            , renderResult "Total Fees" $ formatAmount results.totalFees
            ]
        ]

    renderResult label value =
      HH.div
        [ HP.class_ (HH.ClassName "list-item__content") ]
        [ HH.div [ HP.class_ (HH.ClassName "label") ] [ HH.text label ]
        , HH.div [ HP.class_ (HH.ClassName "value") ] [ HH.text value ]
        ]