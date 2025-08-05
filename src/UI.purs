-- Main UI module for the Feels Protocol application
-- Uses the Protocol API for clean state management and Chart.js for visualization
module UI 
  ( renderUI
  ) where

import Prelude
import Data.Array ((:), length, find, last, head, zip, range, reverse, filter, groupBy, take, null, sortBy, drop)
import Data.Array.NonEmpty as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Traversable (traverse_, traverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.String.Common (trim, joinWith)
import Data.String as String
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Data.Int as Int
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Ref (read, write)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Aff.Util (selectElement)
import Web.DOM.ParentNode (QuerySelector(..))
import Partial.Unsafe (unsafePartial)

-- Import protocol API
import Protocol as P
import Protocol (ProtocolRuntime, ProtocolResult(..), 
                 ProtocolError(..), initProtocol, executeCommand, executeQuery, subscribe)

-- Import types we need for display
import Token (TokenType(..), TokenMetadata)
import LendingRecord (LendingRecord, LendingTerms(..), UnbondingPeriod(..), LendingSide(..), getAvailableAmount)
import FFI (initializePriceChart, checkAndInitializeChart, getElementById, getValue, setTimeout, currentTime, registerRemoteAction, triggerUIAction)
import Data.Nullable (toMaybe)
import Simulation (SimulationConfig, SimulationResults, AccountProfile(..), MarketScenario(..), 
                   TradingAction(..), initSimulationWithLendingBook, executeSimulation, calculateResults)
import Simulation as S
import Oracle (PriceObservation)
import LendingBook (initLendingBook)
import Data.Functor (void)
import Utils (formatAmount, formatPercentage)

--------------------------------------------------------------------------------
-- UI State
--------------------------------------------------------------------------------

type UIState =
  { protocol :: Maybe ProtocolRuntime
  , currentUser :: String
  -- Position Creation
  , inputAmount :: Number
  , selectedAsset :: TokenType
  , collateralAsset :: TokenType
  , unbondingPeriod :: UnbondingPeriod
  , leverage :: Number
  -- Gateway
  , showGateway :: Boolean
  , jitoSOLAmount :: Number
  , feelsSOLAmount :: Number
  -- Wallet
  , jitoSOLBalance :: Number
  , feelsSOLBalance :: Number
  -- Simulation
  , simulationConfig :: SimulationConfig
  , simulationResults :: Maybe SimulationResults
  , simulationRunning :: Boolean
  -- Chart data
  , priceHistory :: Array 
      { timestamp :: Number
      , block :: Int  -- Block number
      , price :: Number  -- JitoSOL/FeelsSOL price
      , nfvValue :: Number  -- NFV floor for FeelsSOL
      , tokens :: Array 
          { ticker :: String
          , price :: Number  -- Token/FeelsSOL price
          , nfvFloor :: Number  -- NFV floor for this token
          , live :: Boolean  -- Whether token is live
          }
      }
  -- Cached data from protocol
  , userTokens :: Array TokenMetadata
  , userPositions :: Array LendingRecord
  , lenderOffers :: Array LendingRecord
  , protocolStats :: Maybe 
      { totalValueLocked :: Number
      , totalUsers :: Int
      , activePositions :: Int
      , liveTokens :: Int
      , totalLenderOffers :: Int
      , nfvBalance :: Number
      , feelsSOLSupply :: Number
      , jitoSOLLocked :: Number
      }
  , loading :: Boolean
  , error :: Maybe String
  -- Token Creation
  , tokenTicker :: String
  , tokenName :: String
  , tokenValidationErrors :: Array String
  }

-- Component Actions
data Action 
  = Initialize
  | RefreshData
  | RenderChart
  -- Position Management
  | UpdateInputAmount Number
  | SelectAsset TokenType
  | SelectCollateralAsset TokenType
  | SetUnbondingPeriod UnbondingPeriod
  | SetLeverage Number
  | CreatePosition
  -- Token Creation
  | CreateTokenUI
  | UpdateTokenTicker String
  | UpdateTokenName String
  -- Gateway
  | ToggleGateway
  | UpdateJitoSOLAmount Number
  | UpdateFeelsSOLAmount Number
  | EnterGateway
  | ExitGateway
  -- Simulation
  | UpdateSimulationConfig (SimulationConfig -> SimulationConfig)
  | RunSimulation

-- Helper function to format chart data points as JSON
formatChartPoint :: { timestamp :: Number, block :: Int, price :: Number, nfvValue :: Number, tokens :: Array { ticker :: String, price :: Number, nfvFloor :: Number, live :: Boolean } } -> String
formatChartPoint point = 
  "{\"timestamp\":" <> show point.timestamp <> 
  ",\"block\":" <> show point.block <> 
  ",\"price\":" <> show point.price <> 
  ",\"nfvValue\":" <> show point.nfvValue <> 
  ",\"tokens\":{" <> 
  (joinWith "," $ map (\t -> "\"" <> t.ticker <> "\":{\"ticker\":\"" <> t.ticker <> "\",\"price\":" <> show t.price <> ",\"nfvFloor\":" <> show t.nfvFloor <> ",\"live\":" <> show t.live <> "}") point.tokens) <>
  "}}"

-- Parse token type from string value
parseTokenType :: String -> TokenType
parseTokenType "JitoSOL" = JitoSOL
parseTokenType "FeelsSOL" = FeelsSOL
parseTokenType ticker = Token ticker

-- Default simulation config
defaultSimulationConfig :: SimulationConfig
defaultSimulationConfig =
  { scenario: BullMarket
  , numAccounts: 5      -- Reduced from 10
  , simulationBlocks: 100
  , initialJitoSOLPrice: 1.05
  , priceVolatility: 0.02
  , accountProfiles: [Whale, Aggressive, Conservative]
  , actionFrequency: 1.0  -- Reduced from 2.0
  , leveragePreference: 0.3
  , stakingPreference: 0.5
  }

-- Component definition
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialUIState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialUIState :: UIState
initialUIState =
  { protocol: Nothing
  , currentUser: "main-user"
  -- Position Creation
  , inputAmount: 100.0
  , selectedAsset: FeelsSOL
  , collateralAsset: JitoSOL
  , unbondingPeriod: Infinite
  , leverage: 2.0
  -- Gateway
  , showGateway: true  -- Always show gateway as requested
  , jitoSOLAmount: 100.0
  , feelsSOLAmount: 100.0
  -- Wallet
  , jitoSOLBalance: 5000.0
  , feelsSOLBalance: 5000.0
  -- Simulation
  , simulationConfig: defaultSimulationConfig
  , simulationResults: Nothing
  , simulationRunning: false
  -- Chart data
  , priceHistory: []
  -- Cached data
  , userTokens: []
  , userPositions: []
  , lenderOffers: []
  , protocolStats: Nothing
  , loading: true
  , error: Nothing
  -- Token Creation
  , tokenTicker: ""
  , tokenName: ""
  , tokenValidationErrors: []
  }

--------------------------------------------------------------------------------
-- Render
--------------------------------------------------------------------------------

render :: forall m. UIState -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "app") ]
    [ -- Main content
      if state.loading
      then HH.div 
        [ HP.class_ (HH.ClassName "loading") ] 
        [ HH.text "Initializing protocol..." ]
      else HH.div
        [ HP.class_ (HH.ClassName "main-layout two-column") ]
        [ -- Left column
          HH.div
            [ HP.class_ (HH.ClassName "left-column") ]
            [ renderSimulationPanel state
            , renderWalletPanel state
            , renderGatewayPanel state
            , renderTokenCreatorPanel state
            , renderUserTokensPanel state.userTokens
            , renderCreatePositionPanel state
            , renderPositionsPanel state.userPositions
            , renderSystemPanel state
            ]
        , -- Right column
          HH.div
            [ HP.class_ (HH.ClassName "right-column") ]
            [ renderPriceChartPanel state
            , renderLoanBookPanel state.lenderOffers
            ]
        ]
    ]

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
          , renderMetric "NFV Balance" $ formatAmount stats.nfvBalance
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

--------------------------------------------------------------------------------
-- Action Handling
--------------------------------------------------------------------------------

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM UIState Action () o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize protocol
    protocol <- H.liftEffect initProtocol
    
    -- Subscribe to protocol state changes
    _ <- H.liftEffect $ subscribe protocol \_ -> do
      log "Protocol state changed, refreshing UI data"
    
    -- Store protocol runtime
    H.modify_ _ { protocol = Just protocol, loading = false }
    
    -- Register remote control actions
    H.liftEffect $ registerRemoteAction "runSimulation" \params -> do
      log "Remote action: runSimulation triggered"
      -- For now, return a success response immediately
      -- The actual simulation will be triggered by the triggerUIAction call
      success <- triggerUIAction "runSimulation"
      pure $ if success
        then { status: "success", message: "Simulation button clicked successfully" }
        else { status: "error", message: "Failed to find or click simulation button" }
    
    H.liftEffect $ registerRemoteAction "refreshData" \_ -> do
      log "Remote action: refreshData triggered"
      void $ triggerUIAction "refreshData"
      pure unit
    
    H.liftEffect $ registerRemoteAction "getChartData" \_ -> do
      log "Remote action: getChartData triggered"
      -- Get chart data from DOM
      chartDataEl <- getElementById "chart-data"
      case toMaybe chartDataEl of
        Just el -> do
          chartData <- getValue el
          log $ "Chart data: " <> chartData
        Nothing -> log "Chart data element not found"
      pure unit
    
    H.liftEffect $ registerRemoteAction "testTokenValidation" \_ -> do
      log "Remote action: testTokenValidation triggered"
      void $ triggerUIAction "testTokenValidation"
      pure unit
    
    -- Initial data fetch
    handleAction RefreshData
  
  RefreshData -> do
    state <- H.get
    case state.protocol of
      Nothing -> pure unit
      Just protocol -> do
        -- Get user positions
        posResult <- H.liftEffect $ executeQuery protocol (P.GetUserPositions state.currentUser)
        case posResult of
          Right (PositionList positions) -> 
            H.modify_ _ { userPositions = positions }
          _ -> pure unit
        
        -- Get lender offers
        offersResult <- H.liftEffect $ executeQuery protocol P.GetLenderOffers
        case offersResult of
          Right (LenderOfferList offers) -> do
            H.liftEffect $ log $ "RefreshData: Found " <> show (length offers) <> " lender offers"
            -- Log details about each offer for debugging
            H.liftEffect $ traverse_ (\offer -> 
              log $ "  Offer #" <> show offer.id <> ": " <> 
                    show offer.lendAmount <> " " <> show offer.lendAsset <> 
                    " (status: " <> show offer.status <> ")"
            ) offers
            H.modify_ _ { lenderOffers = offers }
          Left err -> 
            H.liftEffect $ log $ "RefreshData: Failed to get lender offers: " <> show err
          _ -> pure unit
        
        -- Get protocol stats
        statsResult <- H.liftEffect $ executeQuery protocol P.GetProtocolStats
        case statsResult of
          Right (ProtocolStatsResult stats) -> do
            H.liftEffect $ log $ "RefreshData: Updated protocol stats - TVL: " <> show stats.totalValueLocked <> ", Users: " <> show stats.totalUsers
            H.modify_ _ { protocolStats = Just stats }
          Left err ->
            H.liftEffect $ log $ "RefreshData: Failed to get protocol stats: " <> show err
          _ -> pure unit
        
        -- Get wallet balances
        jitoBalanceResult <- H.liftEffect $ executeQuery protocol (P.GetUserBalance state.currentUser JitoSOL)
        case jitoBalanceResult of
          Right (P.Balance balance) ->
            H.modify_ _ { jitoSOLBalance = balance }
          _ -> pure unit
        
        feelsBalanceResult <- H.liftEffect $ executeQuery protocol (P.GetUserBalance state.currentUser FeelsSOL)
        case feelsBalanceResult of
          Right (P.Balance balance) ->
            H.modify_ _ { feelsSOLBalance = balance }
          _ -> pure unit
        
        -- Clear any errors
        H.modify_ _ { error = Nothing }
  
  RenderChart -> do
    H.liftEffect $ log "Rendering price chart..."
    currentState <- H.get
    H.liftEffect $ log $ "Price history length: " <> show (length currentState.priceHistory)
    H.liftEffect $ log $ "Chart data JSON: " <> ("[" <> (joinWith "," $ map formatChartPoint currentState.priceHistory) <> "]")
    -- Log first data point to check structure
    case head currentState.priceHistory of
      Just firstPoint -> H.liftEffect $ log $ "First data point has " <> show (length firstPoint.tokens) <> " tokens"
      Nothing -> H.liftEffect $ log "No price history data"
    -- Add a delay before calling the chart initialization
    H.liftEffect $ log "UI: About to schedule checkAndInitializeChart"
    H.liftEffect $ void $ setTimeout checkAndInitializeChart 250
    H.liftEffect $ log "UI: Scheduled checkAndInitializeChart for 250ms delay"

  UpdateInputAmount amount -> do
    H.modify_ _ { inputAmount = amount }
  
  SelectAsset asset -> do
    H.modify_ _ { selectedAsset = asset }

  SelectCollateralAsset asset -> do
    H.modify_ _ { collateralAsset = asset }
  
  SetUnbondingPeriod period -> do
    H.modify_ _ { unbondingPeriod = period }

  SetLeverage lev -> do
    H.modify_ _ { leverage = lev }
  
  UpdateTokenTicker ticker -> do
    state <- H.get
    let errors = validateTokenInput ticker state.tokenName state.userTokens
    -- Debug logging
    H.liftEffect $ do
      log $ "Ticker validation: " <> ticker <> " -> errors: " <> show (length errors)
      traverse_ (log <<< ("  - " <> _)) errors
    H.modify_ _ { tokenTicker = ticker, tokenValidationErrors = errors }
  
  UpdateTokenName name -> do
    state <- H.get
    let errors = validateTokenInput state.tokenTicker name state.userTokens  
    -- Debug logging
    H.liftEffect $ do
      log $ "Name validation: " <> name <> " -> errors: " <> show (length errors)
      traverse_ (log <<< ("  - " <> _)) errors
    H.modify_ _ { tokenName = name, tokenValidationErrors = errors }

  CreateTokenUI -> do
    state <- H.get
    case state.protocol of
      Nothing -> pure unit
      Just protocol -> do
        let ticker = trim state.tokenTicker
            name = trim state.tokenName
            
        if null state.tokenValidationErrors && ticker /= "" && name /= ""
          then do
            -- Execute create token command
            result <- H.liftEffect $ executeCommand protocol 
              (P.CreateToken state.currentUser ticker name)
            
            case result of
              Right (TokenCreated token) -> do
                H.liftEffect $ log $ "Token created: " <> token.ticker
                -- Reset form and clear validation errors
                H.modify_ \s -> s 
                  { userTokens = token : s.userTokens
                  , tokenTicker = ""
                  , tokenName = ""
                  , tokenValidationErrors = []
                  , error = Nothing
                  }
                -- Refresh all data
                handleAction RefreshData
              Right _ -> 
                H.modify_ _ { error = Just "Unexpected result from token creation" }
              Left err -> 
                H.modify_ _ { error = Just $ show err }
          else 
            H.modify_ _ { error = Just "Please fix validation errors before creating token" }
  
  CreatePosition -> do
    state <- H.get
    case state.protocol of
      Nothing -> pure unit
      Just protocol -> do
        -- Determine terms based on which fields are filled
        let terms = 
              if state.leverage > 1.0 
                then LeverageTerms state.leverage
                else if state.unbondingPeriod == Infinite
                  then SwapTerms
                  else StakingTerms state.unbondingPeriod
            
            collateralAmount = state.inputAmount * 1.5  -- Simplified
        
        -- Execute create position command
        result <- H.liftEffect $ executeCommand protocol
          (P.CreateLendingPosition 
            state.currentUser
            state.selectedAsset
            state.inputAmount
            state.collateralAsset
            collateralAmount
            terms
            Nothing)
        
        case result of
          Right (PositionCreated pos) -> do
            H.liftEffect $ log $ "Position created: #" <> show pos.id
            -- Refresh all data
            handleAction RefreshData
          Right _ ->
            H.modify_ _ { error = Just "Unexpected result from position creation" }
          Left err ->
            H.modify_ _ { error = Just $ show err }
  
  ToggleGateway -> do
    H.modify_ \s -> s { showGateway = not s.showGateway }

  UpdateJitoSOLAmount amount -> do
    H.modify_ _ { jitoSOLAmount = amount }

  UpdateFeelsSOLAmount amount -> do
    H.modify_ _ { feelsSOLAmount = amount }

  EnterGateway -> do
    state <- H.get
    case state.protocol of
      Nothing -> pure unit
      Just protocol -> do
        result <- H.liftEffect $ executeCommand protocol
          (P.EnterGateway state.currentUser state.jitoSOLAmount)
        
        case result of
          Right (GatewayEntered info) -> do
            H.liftEffect $ log $ "Entered gateway: " <> show info.feelsSOLMinted <> " FeelsSOL minted"
            handleAction RefreshData
          Right _ ->
            H.modify_ _ { error = Just "Unexpected result from gateway entry" }
          Left err ->
            H.modify_ _ { error = Just $ show err }

  ExitGateway -> do
    state <- H.get
    case state.protocol of
      Nothing -> pure unit
      Just protocol -> do
        -- For exit, we use the input amount as FeelsSOL amount to convert
        result <- H.liftEffect $ executeCommand protocol
          (P.ExitGateway state.currentUser state.feelsSOLAmount)
        
        case result of
          Right (GatewayExited info) -> do
            H.liftEffect $ log $ "Exited gateway: " <> show info.jitoSOLReceived <> " JitoSOL received"
            handleAction RefreshData
          Right _ ->
            H.modify_ _ { error = Just "Unexpected result from gateway exit" }
          Left err ->
            H.modify_ _ { error = Just $ show err }

  UpdateSimulationConfig updater -> do
    H.modify_ \s -> s { simulationConfig = updater s.simulationConfig }

  RunSimulation -> do
    H.liftEffect $ log "Starting simulation..."
    H.modify_ _ { simulationRunning = true, error = Nothing }
    
    state <- H.get
    case state.protocol of
      Nothing -> H.modify_ _ { error = Just "Protocol not initialized" }
      Just protocol -> do
        -- Extract the protocol's lending book instead of creating a new one
        protocolState <- H.liftEffect $ read protocol.state
        
        -- Initialize simulation with the protocol's actual lending book and oracle
        simState <- H.liftEffect $ initSimulationWithLendingBook 
          state.simulationConfig 
          protocolState.lendingBook
          protocolState.oracle
        
        -- Run simulation
        finalState <- H.liftEffect $ executeSimulation state.simulationConfig simState
        
        -- Calculate results
        results <- H.liftEffect $ calculateResults state.simulationConfig finalState
        
        H.liftEffect $ log $ "Simulation completed with " <> show results.totalUsers <> " users"
        
        -- Process token creation actions from the simulation
        let tokenCreationActions = filter isCreateTokenAction finalState.actionHistory
        H.liftEffect $ log $ "Processing " <> show (length tokenCreationActions) <> " token creation actions"
        
        _ <- traverse (processTokenCreation protocol) tokenCreationActions
        
        -- Get all tokens from the protocol (including those created during simulation)
        allTokensResult <- H.liftEffect $ executeQuery protocol P.GetAllTokens
        
        let allTokens = case allTokensResult of
              Right (TokenList tokens) -> tokens
              _ -> []
            
            -- Include ALL live tokens (including system tokens) in the price history
            -- But exclude FeelsSOL (can't pair with itself) and duplicates from old casing
            liveTokens = filter (\t -> t.live && 
                                         t.ticker /= "FeelsSOL" &&  -- Exclude FeelsSOL - can't pair with itself
                                         t.ticker /= "jitoSOL" && 
                                         t.ticker /= "feelsSOL") allTokens
        
        H.liftEffect $ log $ "Found " <> show (length allTokens) <> " total tokens, " <> show (length liveTokens) <> " live tokens"
        
        -- Get the oracle's actual price history
        oracleState <- H.liftEffect $ read protocolState.oracle.state
        let oraclePriceHistory = oracleState.priceHistory
        
        H.liftEffect $ log $ "Oracle has " <> show (length oraclePriceHistory) <> " price observations"
        
        -- Convert oracle price history to chart format
        -- Sort observations by block number (not timestamp) to ensure proper ordering
        let sortedHistory = sortBy (\a b -> compare (fromMaybe 0 a.block) (fromMaybe 0 b.block)) oraclePriceHistory
            
            -- Debug: Log block numbers to see if we have all blocks
            blockNumbers = map (fromMaybe (-1)) $ map _.block sortedHistory
        
        H.liftEffect $ log $ "Sorted history blocks: " <> show (take 20 blockNumbers) <> 
                            " ... " <> show (drop (length blockNumbers - 5) blockNumbers)
        
        -- Check for gaps in block numbers
        let findFirstGap nums = case nums of
              [] -> Nothing
              [_] -> Nothing
              _ -> 
                let pairs = zip nums (drop 1 nums)
                    gapPairs = filter (\(Tuple a b) -> b - a > 1) pairs
                in case head gapPairs of
                     Just (Tuple a b) -> Just ("Gap from block " <> show a <> " to " <> show b)
                     Nothing -> Nothing
        
        case findFirstGap blockNumbers of
          Just gap -> H.liftEffect $ log $ "WARNING: Found gap in block sequence: " <> gap
          Nothing -> pure unit
            
        -- Group observations by block number instead of timestamp
        -- This ensures we get one chart point per block
        let groupByBlock obs = fromMaybe 0 obs.block
            grouped = groupBy (\a b -> groupByBlock a == groupByBlock b) sortedHistory
            
            -- Convert each group of observations to a chart point
            -- First filter to ensure we have valid groups, then map
            validGroups = filter (\neGroup -> 
              case head (NEA.toArray neGroup) of
                Just obs -> obs.timestamp > 0.0
                Nothing -> false
            ) grouped
            
            convertedPriceHistory = map (\neGroup ->
              let group = NEA.toArray neGroup
                  timestamp = case head group of
                    Just obs -> obs.timestamp
                    Nothing -> 0.0  -- This shouldn't happen due to filter above
                  
                  -- Get block number from first observation
                  blockNum = case head group of
                    Just obs -> fromMaybe 0 obs.block  -- Default to 0 if no block
                    Nothing -> 0
                  
                  -- Find JitoSOL price in this group
                  jitoSOLObs = find (\obs -> obs.baseAsset == JitoSOL) group
                  jitoPrice = case jitoSOLObs of
                    Just obs -> obs.impliedPrice
                    Nothing -> 1.05  -- Default
                  
                  -- Get prices for all live tokens
                  tokenPrices = map (\token ->
                    let tokenObs = find (\obs ->
                          case obs.baseAsset of
                            Token t -> t == token.ticker
                            _ -> false
                        ) group
                    in { ticker: token.ticker
                       , price: case tokenObs of
                           Just obs -> obs.impliedPrice
                           Nothing -> 0.0
                       , nfvFloor: case tokenObs of
                           Just obs -> obs.nfvFloor
                           Nothing -> 0.0
                       , live: token.live
                       }
                  ) liveTokens
              in { timestamp: timestamp
                 , block: blockNum
                 , price: jitoPrice
                 , nfvValue: jitoPrice * 0.98  -- NFV tracks slightly below price
                 , tokens: tokenPrices
                 }
            ) validGroups
        
        H.liftEffect $ log $ "Converted " <> show (length validGroups) <> " valid price groups to chart format (from " <> show (length grouped) <> " total)"
        
        -- Since the simulation used the protocol's actual lending book (shared by reference),
        -- the protocol's lending book is already updated. No need to write back the state.
        H.liftEffect $ log "Simulation used shared lending book - protocol state automatically updated"
        
        -- Refresh data to show updated offers
        handleAction RefreshData
        
        -- Update state
        H.modify_ _ 
          { simulationRunning = false
          , simulationResults = Just results
          , priceHistory = convertedPriceHistory
          }
        
        -- Force a render cycle before initializing chart
        H.liftEffect $ log "State updated, scheduling chart render..."
        
        -- Use a small delay to ensure Halogen completes the DOM update
        void $ H.fork do
          H.liftEffect $ void $ setTimeout (pure unit) 200
          handleAction RenderChart
  where
    isCreateTokenAction action = case action of
      S.CreateToken _ _ _ -> true
      _ -> false
    
    processTokenCreation protocol action = case action of
      S.CreateToken userId ticker name -> H.liftEffect $ do
        log $ "Creating token: " <> ticker <> " for user: " <> userId
        result <- executeCommand protocol (P.CreateToken userId ticker name)
        case result of
          Right _ -> log $ "Successfully created token: " <> ticker
          Left err -> log $ "Failed to create token: " <> show err
      _ -> pure unit

--------------------------------------------------------------------------------
-- Token validation function
validateTokenInput :: String -> String -> Array TokenMetadata -> Array String
validateTokenInput ticker name existingTokens =
  let trimmedTicker = trim ticker
      trimmedName = trim name
      
      -- Check ticker length
      tickerLengthErrors = 
        if trimmedTicker == "" then []
        else if String.length trimmedTicker < 3 then ["Ticker must be at least 3 characters"]
        else if String.length trimmedTicker > 10 then ["Ticker must be at most 10 characters"]
        else []
      
      -- Check for duplicate ticker
      duplicateTickerErrors = 
        if trimmedTicker == "" then []
        else case find (\t -> t.ticker == trimmedTicker) existingTokens of
          Just _ -> ["Token with ticker '" <> trimmedTicker <> "' already exists"]
          Nothing -> []
      
      -- Check for duplicate name  
      duplicateNameErrors =
        if trimmedName == "" then []
        else case find (\t -> t.name == trimmedName) existingTokens of
          Just _ -> ["Token with name '" <> trimmedName <> "' already exists"]
          Nothing -> []
          
      -- Reserved ticker checks
      reservedTickerErrors =
        if trimmedTicker == "" then []
        else if String.toUpper trimmedTicker == "SOL" || 
                String.toUpper trimmedTicker == "JITO" ||
                String.toUpper trimmedTicker == "FEELSSOL" ||
                String.toUpper trimmedTicker == "JITOSOL"
             then ["Ticker '" <> trimmedTicker <> "' is reserved"]
             else []
             
  in tickerLengthErrors <> duplicateTickerErrors <> duplicateNameErrors <> reservedTickerErrors

--------------------------------------------------------------------------------
-- Main Entry Point
--------------------------------------------------------------------------------

renderUI :: Effect Unit
renderUI = do
  log "Starting Feels Protocol UI..."
  
  HA.runHalogenAff do
    appElement <- selectElement (QuerySelector "#app")
    case appElement of
      Nothing -> do
        H.liftEffect $ log "Could not find #app element"
        HA.awaitBody >>= runUI component unit
      Just element -> do
        H.liftEffect $ log "Mounting UI component"
        runUI component unit element