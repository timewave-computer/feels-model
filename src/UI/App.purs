-- UI App Component for the Feels Protocol application
-- Contains the main component definition, layout structure, and entry point
module UI.App
  ( component
  , renderUI
  ) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Aff.Util (selectElement)
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))

-- Import UI modules
import UI.State (UIState, Action(..), initialUIState)
import UI.Action (handleAction)
import UI.Component (renderSystemPanel, renderFeelsSOLPanel, renderWalletPanel, renderTokenCreatorPanel, renderUserTokensPanel, renderCreatePositionPanel, renderLoanBookPanel, renderPositionsPanel, renderSimulationPanel)
-- Removed unused import

--------------------------------------------------------------------------------
-- Main Component Definition
--------------------------------------------------------------------------------

-- Component definition with AppRuntime integration
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialUIState
    , render: render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

--------------------------------------------------------------------------------
-- Main Layout and Rendering
--------------------------------------------------------------------------------

-- Main render function with layout structure
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
            [ renderWalletPanel state
            , renderFeelsSOLPanel state
            , renderTokenCreatorPanel state
            , renderUserTokensPanel state.userTokens
            , renderCreatePositionPanel state
            , renderPositionsPanel state.userPositions
            , renderSimulationPanel state
            , renderSystemPanel state
            ]
        , -- Right column
          HH.div
            [ HP.class_ (HH.ClassName "right-column") ]
            [ -- Hidden data element for chart
              HH.div
                [ HP.id "chart-data-hidden"
                , HP.style "display: none;"
                ]
                [ HH.text "" ]  -- Chart data will be populated by JavaScript
            , -- Chart canvas
              HH.div
                [ HP.class_ (HH.ClassName "panel") ]
                [ HH.h2_ [ HH.text "Price Chart" ]
                , HH.canvas
                    [ HP.id "price-chart"
                    , HP.width 600
                    , HP.height 400
                    ]
                ]
            , renderLoanBookPanel state.lenderOffers
            ]
        ]
    ]

--------------------------------------------------------------------------------
-- Application Entry Point
--------------------------------------------------------------------------------

-- Main entry point for the UI application
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

  where
    initState = do
      log "Initializing protocol state..."
      initState