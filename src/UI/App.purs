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
import UI.Actions (handleAction)
import UI.Components (renderSystemPanel, renderGatewayPanel, renderWalletPanel, renderTokenCreatorPanel, renderUserTokensPanel, renderCreatePositionPanel, renderLoanBookPanel, renderPositionsPanel, renderPriceChartPanel, renderSimulationPanel)

--------------------------------------------------------------------------------
-- Main Component Definition
--------------------------------------------------------------------------------

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