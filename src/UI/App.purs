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
import UI.Components (renderSystemPanel, renderFeelsSOLPanel, renderWalletPanel, renderTokenCreatorPanel, renderUserTokensPanel, renderCreatePositionPanel, renderLoanBookPanel, renderPositionsPanel)
import UI.ProtocolState (AppRuntime, initState)

--------------------------------------------------------------------------------
-- Main Component Definition
--------------------------------------------------------------------------------

-- Component definition with AppRuntime integration
component :: forall q i o m. MonadAff m => AppRuntime -> H.Component q i o m
component appRuntime =
  H.mkComponent
    { initialState: \_ -> initialUIState
    , render: render appRuntime
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction appRuntime
        , initialize = Just Initialize
        }
    }

--------------------------------------------------------------------------------
-- Main Layout and Rendering
--------------------------------------------------------------------------------

-- Main render function with layout structure
render :: forall m. AppRuntime -> UIState -> H.ComponentHTML Action () m
render appRuntime state =
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
            [ renderWalletPanel appRuntime state
            , renderFeelsSOLPanel appRuntime state
            , renderTokenCreatorPanel appRuntime state
            , renderUserTokensPanel appRuntime
            , renderCreatePositionPanel appRuntime state
            , renderPositionsPanel appRuntime
            , renderSystemPanel appRuntime state
            ]
        , -- Right column
          HH.div
            [ HP.class_ (HH.ClassName "right-column") ]
            [ renderLoanBookPanel appRuntime
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
  
  -- Initialize application state
  appRuntime <- H.liftEffect initState
  
  HA.runHalogenAff do
    appElement <- selectElement (QuerySelector "#app")
    case appElement of
      Nothing -> do
        H.liftEffect $ log "Could not find #app element"
        HA.awaitBody >>= runUI (component appRuntime) unit
      Just element -> do
        H.liftEffect $ log "Mounting UI component"
        runUI (component appRuntime) unit element

  where
    initState = do
      log "Initializing protocol state..."
      initState