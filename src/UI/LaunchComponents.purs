-- | UI Components for the Token Launch System
module UI.LaunchComponents where

import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Array (length, head, filter)
import Unsafe.Coerce (unsafeCoerce)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- import Protocol.Launch.Launch (LaunchPhase(..), BatchResult) -- Module not found

-- Temporary types until Launch modules are created
data LaunchPhase = LaunchPhaseA | LaunchPhaseB
type BatchResult = { success :: Boolean }
-- import Utils (formatAmount) -- Module not found

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
  HH.div
    [ HP.class_ (HH.ClassName "launch-creator panel") ]
    [ HH.h3_ [ HH.text "Create Token Launch" ]
    , HH.p 
        [ HP.class_ (HH.ClassName "info-text") ]
        [ HH.text "Launch your token through our cascading batch auction system" ]
    -- Token creation form would go here, reusing existing token creation UI
    , HH.div
        [ HP.class_ (HH.ClassName "launch-info") ]
        [ HH.ul_
            [ HH.li_ [ HH.text "Weekly Phase: 7-day commitment, lowest price" ]
            , HH.li_ [ HH.text "Daily Phase: 1-day commitment, +20% premium" ]
            , HH.li_ [ HH.text "Hourly Phase: 1-hour commitment, +10% premium" ]
            , HH.li_ [ HH.text "Spot Phase: Immediate liquidity, +5% premium" ]
            ]
        ]
    ]

--------------------------------------------------------------------------------
-- Launch Bid Submission Component
--------------------------------------------------------------------------------

renderBidSubmission :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderBidSubmission state =
  HH.div
    [ HP.class_ (HH.ClassName "bid-submission panel") ]
    [ HH.h3_ [ HH.text "Submit Launch Bid" ]
    , case state.selectedLaunch of
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
    , HH.div
        [ HP.class_ (HH.ClassName "price-info") ]
        [ HH.p_ [ HH.text $ "Current Base Price: " <> formatAmount launch.currentPrice <> " FeelsSOL" ]
        , HH.p_ [ HH.text $ "Last Batch Priority Fee: " <> formatPercent launch.lastBatchFeeRatio ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "input-group") ]
        [ HH.label_ [ HH.text "Bid Amount (FeelsSOL)" ]
        , HH.input
            [ HP.type_ HP.InputNumber
            , HP.value $ show state.bidAmount
            , HP.min 0.0
            , HP.step (HP.Step 10.0)
            , HE.onValueInput \v -> UpdateBidAmount (parseNumber v)
            ]
        ]
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
    , HH.div
        [ HP.class_ (HH.ClassName "bid-summary") ]
        [ HH.p_ [ HH.text $ "Base Payment: " <> formatAmount state.bidAmount <> " FeelsSOL" ]
        , HH.p_ [ HH.text $ "Priority Fee: " <> formatAmount (state.bidAmount * state.priorityFeePercent / 100.0) <> " FeelsSOL" ]
        , HH.hr_
        , HH.p 
            [ HP.class_ (HH.ClassName "total") ]
            [ HH.text $ "Total Payment: " <> formatAmount (state.bidAmount * (1.0 + state.priorityFeePercent / 100.0)) <> " FeelsSOL" ]
        ]
    , HH.button
        [ HP.class_ (HH.ClassName "btn primary submit-bid")
        , HP.disabled (state.bidAmount <= 0.0)
        , HE.onClick \_ -> SubmitBid
        ]
        [ HH.text "Submit Bid" ]
    ]

--------------------------------------------------------------------------------
-- Active Launches Dashboard
--------------------------------------------------------------------------------

renderActiveLaunches :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderActiveLaunches state =
  HH.div
    [ HP.class_ (HH.ClassName "active-launches panel") ]
    [ HH.div
        [ HP.class_ (HH.ClassName "panel-header") ]
        [ HH.h3_ [ HH.text "Active Launches" ]
        , HH.button
            [ HP.class_ (HH.ClassName "btn secondary refresh")
            , HE.onClick \_ -> RefreshLaunches
            ]
            [ HH.text "Refresh" ]
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
        [ HH.div_
            [ HH.span 
                [ HP.class_ (HH.ClassName "label") ]
                [ HH.text "Price" ]
            , HH.span 
                [ HP.class_ (HH.ClassName "value") ]
                [ HH.text $ formatAmount launch.currentPrice ]
            ]
        , HH.div_
            [ HH.span 
                [ HP.class_ (HH.ClassName "label") ]
                [ HH.text "Progress" ]
            , HH.span 
                [ HP.class_ (HH.ClassName "value") ]
                [ HH.text $ formatPercent (launch.totalDistributed / launch.totalSupply) ]
            ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "progress-bar") ]
        [ HH.div
            [ HP.class_ (HH.ClassName "progress-fill")
            , HP.style $ "width: " <> show (launch.totalDistributed / launch.totalSupply * 100.0) <> "%"
            ]
            []
        ]
    , HH.button
        [ HP.class_ (HH.ClassName "btn link view-history")
        , HE.onClick \_ -> ViewBatchHistory launch.launchId
        ]
        [ HH.text "View History" ]
    ]

--------------------------------------------------------------------------------
-- Batch History Component
--------------------------------------------------------------------------------

renderBatchHistory :: forall m. LaunchUIState -> H.ComponentHTML LaunchAction () m
renderBatchHistory state =
  HH.div
    [ HP.class_ (HH.ClassName "batch-history panel") ]
    [ HH.h3_ [ HH.text "Batch Auction History" ]
    , if length state.launchHistory == 0
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
    toNumber n = unsafeCoerce n

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

phaseClass :: LaunchPhase -> String
phaseClass = case _ of
  WeeklyPhase -> "phase-weekly"
  DailyPhase -> "phase-daily"
  HourlyPhase -> "phase-hourly"
  SpotPhase -> "phase-spot"
  Completed -> "phase-completed"

formatPercent :: Number -> String
formatPercent n = show (n * 100.0) <> "%"

parseNumber :: String -> Number
parseNumber s = case parseFloat s of
  n | isNaN n -> 0.0
  n -> n
  where
    parseFloat :: String -> Number
    parseFloat = unsafeCoerce
    
    isNaN :: Number -> Boolean
    isNaN n = n /= n
