-- | UI Components for the Token Launch System
module UI.LaunchComponents where

import Prelude
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Array (length, head, filter)
import Data.Number as Number
import UI.Util.Codecs (intToNumber)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Protocol.LaunchVault (LaunchPhase(..))
import Utils (formatAmount)
import UI.Component.FormElements (renderNumberInput, renderButton, renderFormGroup, defaultNumberInputConfig, defaultButtonConfig, ButtonStyle(..))
import UI.Component.Panel (renderPanel, renderSection, defaultPanelConfig, defaultSectionConfig, PanelStyle(..))
import UI.Component.DataDisplay (renderMetric, formatPercentage)

-- LaunchPhase imported from Protocol.LaunchVault
type BatchResult = 
  { success :: Boolean
  , batchNumber :: Int
  , winners :: Array String
  , basePayment :: Number
  , avgPriorityFeeRatio :: Number
  , protocolRevenue :: Number
  }

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type LaunchUIState =
  { selectedLaunch :: Maybe String
  , bidAmount :: Number
  , priorityFeePercent :: Number
  , activeLaunches :: Array LaunchInfo
  , launchHistory :: Array BatchResult
  }

type LaunchInfo =
  { launchId :: String
  , tokenTicker :: String
  , currentPhase :: LaunchPhase
  , currentPrice :: Number
  , totalDistributed :: Number
  , totalSupply :: Number
  , lastBatchFeeRatio :: Number
  }

data LaunchAction
  = SelectLaunch String
  | UpdateBidAmount Number
  | UpdatePriorityFee Number
  | SubmitBid
  | RefreshLaunches
  | ViewBatchHistory String

--------------------------------------------------------------------------------
-- Launch Creation Component
--------------------------------------------------------------------------------

renderLaunchCreator :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderLaunchCreator _ =
  renderPanel (defaultPanelConfig { title = Just "Create Token Launch", style = Standard })
    [ HH.p 
        [ HP.class_ (HH.ClassName "info-text") ]
        [ HH.text "Launch your token through our cascading batch auction system" ]
    -- Token creation form would go here, reusing existing token creation UI
    , renderSection (defaultSectionConfig { title = Just "Launch Information" })
        [ HH.ul_
            [ HH.li_ [ HH.text "Monthly Phase: 28-day commitment, lowest price" ]
            , HH.li_ [ HH.text "Swap Phase: Immediate liquidity, higher price" ]
            ]
        ]
    ]

--------------------------------------------------------------------------------
-- Launch Bid Submission Component
--------------------------------------------------------------------------------

renderBidSubmission :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderBidSubmission state =
  renderPanel (defaultPanelConfig { title = Just "Submit Launch Bid", style = Standard })
    [ case state.selectedLaunch of
        Nothing -> 
          HH.p 
            [ HP.class_ (HH.ClassName "info-text") ]
            [ HH.text "Select an active launch to submit a bid" ]
        Just launchId ->
          case findLaunch launchId state.activeLaunches of
            Nothing -> HH.text ""
            Just launch -> renderBidForm launch state
    ]
  where
    findLaunch :: String -> Array LaunchInfo -> Maybe LaunchInfo
    findLaunch lid launches = head $ filter (\l -> l.launchId == lid) launches

renderBidForm :: forall m. LaunchInfo -> LaunchUIState -> H.ComponentHTML LaunchAction () m  
renderBidForm launch state =
  HH.div
    [ HP.class_ (HH.ClassName "bid-form") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "launch-info-header") ]
        [ HH.h4_ [ HH.text $ launch.tokenTicker <> " Launch" ]
        , HH.span 
            [ HP.class_ (HH.ClassName $ "phase-badge " <> phaseClass launch.currentPhase) ]
            [ HH.text $ show launch.currentPhase ]
        ]
    , renderSection (defaultSectionConfig { title = Just "Price Information" })
        [ renderMetric "Current Base Price" (formatAmount launch.currentPrice <> " FeelsSOL")
        , renderMetric "Last Batch Priority Fee" (formatPercent launch.lastBatchFeeRatio)
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "form-inputs") ]
        [ renderNumberInput (defaultNumberInputConfig
            { id = "bid-amount"
            , name = "bid-amount"
            , placeholder = "Enter amount"
            , value = state.bidAmount
            , onChange = UpdateBidAmount
            , label = Just "Bid Amount (FeelsSOL)"
            , min = Just 0.0
            , step = Just 10.0
            , required = true
            })
        , HH.div
            [ HP.class_ (HH.ClassName "input-group") ]
            [ HH.label_ [ HH.text $ "Priority Fee: " <> formatPercent (state.priorityFeePercent / 100.0) ]
            , HH.input
                [ HP.type_ HP.InputRange
                , HP.value $ show state.priorityFeePercent
                , HP.min 0.0
                , HP.max 50.0
                , HP.step (HP.Step 1.0)
                , HE.onValueInput \v -> UpdatePriorityFee (parseNumber v)
                ]
            , HH.div
                [ HP.class_ (HH.ClassName "fee-labels") ]
                [ HH.span_ [ HH.text "0%" ]
                , HH.span_ [ HH.text "25%" ]
                , HH.span_ [ HH.text "50%" ]
                ]
            ]
        ]
    , renderSection (defaultSectionConfig { title = Just "Bid Summary" })
        [ renderMetric "Base Payment" (formatAmount state.bidAmount <> " FeelsSOL")
        , renderMetric "Priority Fee" (formatAmount (state.bidAmount * state.priorityFeePercent / 100.0) <> " FeelsSOL")
        , HH.hr_
        , HH.p 
            [ HP.class_ (HH.ClassName "total") ]
            [ HH.text $ "Total Payment: " <> formatAmount (state.bidAmount * (1.0 + state.priorityFeePercent / 100.0)) <> " FeelsSOL" ]
        ]
    , renderButton (defaultButtonConfig
        { text = "Submit Bid"
        , onClick = SubmitBid
        , style = Primary
        , disabled = state.bidAmount <= 0.0
        })
    ]

--------------------------------------------------------------------------------
-- Active Launches Dashboard
--------------------------------------------------------------------------------

renderActiveLaunches :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderActiveLaunches state =
  renderPanel (defaultPanelConfig { title = Just "Active Launches", style = Standard })
    [ HH.div
        [ HP.class_ (HH.ClassName "panel-actions") ]
        [ renderButton (defaultButtonConfig
            { text = "Refresh"
            , onClick = RefreshLaunches
            , style = Secondary
            })
        ]
    , if length state.activeLaunches == 0
        then HH.p 
          [ HP.class_ (HH.ClassName "info-text") ]
          [ HH.text "No active launches at this time" ]
        else HH.div
          [ HP.class_ (HH.ClassName "launch-grid") ]
          (map renderLaunchCard state.activeLaunches)
    ]

renderLaunchCard :: forall m. LaunchInfo -> H.ComponentHTML LaunchAction () m
renderLaunchCard launch =
  HH.div
    [ HP.class_ (HH.ClassName "launch-card")
    , HE.onClick \_ -> SelectLaunch launch.launchId
    ]
    [ HH.div
        [ HP.class_ (HH.ClassName "launch-header") ]
        [ HH.h4_ [ HH.text launch.tokenTicker ]
        , HH.span 
            [ HP.class_ (HH.ClassName $ "phase-badge " <> phaseClass launch.currentPhase) ]
            [ HH.text $ show launch.currentPhase ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "launch-stats") ]
        [ renderMetric "Price" (formatAmount launch.currentPrice)
        , renderMetric "Progress" (formatPercent (launch.totalDistributed / launch.totalSupply))
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "progress-bar") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "progress-fill")
            , HP.style $ "width: " <> show (launch.totalDistributed / launch.totalSupply * 100.0) <> "%"
            ]
            []
        ]
    , renderButton (defaultButtonConfig
        { text = "View History"
        , onClick = ViewBatchHistory launch.launchId
        , style = Secondary
        , className = "btn--link"
        })
    ]

--------------------------------------------------------------------------------
-- Batch History Component
--------------------------------------------------------------------------------

renderBatchHistory :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderBatchHistory state =
  renderPanel (defaultPanelConfig { title = Just "Batch Auction History", style = Standard })
    [ if length state.launchHistory == 0
        then HH.p 
          [ HP.class_ (HH.ClassName "info-text") ]
          [ HH.text "No batch history available" ]
        else HH.table
          [ HP.class_ (HH.ClassName "history-table") ]
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Batch #" ]
                  , HH.th_ [ HH.text "Winners" ]
                  , HH.th_ [ HH.text "Base Price" ]
                  , HH.th_ [ HH.text "Avg Priority Fee" ]
                  , HH.th_ [ HH.text "Protocol Revenue" ]
                  ]
              ]
          , HH.tbody_
              (map renderBatchRow state.launchHistory)
          ]
    ]

renderBatchRow :: forall m. BatchResult -> H.ComponentHTML LaunchAction () m
renderBatchRow batch =
  HH.tr_
    [ HH.td_ [ HH.text $ show batch.batchNumber ]
    , HH.td_ [ HH.text $ show (length batch.winners) ]
    , HH.td_ [ HH.text $ formatAmount (batch.basePayment / toNumber (length batch.winners)) ]
    , HH.td_ [ HH.text $ formatPercent batch.avgPriorityFeeRatio ]
    , HH.td_ [ HH.text $ formatAmount batch.protocolRevenue ]
    ]
  where
    toNumber :: Int -> Number
    toNumber n = intToNumber n

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

phaseClass :: LaunchPhase -> String
phaseClass = case _ of
  MonthlyPhase -> "phase-monthly"
  SwapPhase -> "phase-swap"
  Completed -> "phase-completed"

formatPercent :: Number -> String
formatPercent = formatPercentage

parseNumber :: String -> Number  
parseNumber s = fromMaybe 0.0 (Number.fromString s)
