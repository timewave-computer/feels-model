-- | Reusable Panel Components for Feels Protocol UI
-- | 
-- | This module provides standardized panel and section components to reduce
-- | code duplication and ensure consistent layout across the application.
module UI.Component.Panel
  ( -- Panel components
    renderPanel
  , renderSection
  , renderCard
  -- Configuration types
  , PanelConfig
  , SectionConfig
  , CardConfig
  , PanelStyle(..)
  -- Default configurations
  , defaultPanelConfig
  , defaultSectionConfig
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Panel styling options
data PanelStyle
  = DefaultPanel
  | PrimaryPanel
  | SecondaryPanel
  | InfoPanel
  | WarningPanel
  | ErrorPanel
  | Standard

derive instance eqPanelStyle :: Eq PanelStyle

instance showPanelStyle :: Show PanelStyle where
  show DefaultPanel = "panel"
  show PrimaryPanel = "panel panel--primary" 
  show SecondaryPanel = "panel panel--secondary"
  show InfoPanel = "panel panel--info"
  show WarningPanel = "panel panel--warning"
  show ErrorPanel = "panel panel--error"
  show Standard = "panel panel--standard"

-- | Configuration for panel components
type PanelConfig action =
  { title :: Maybe String
  , style :: PanelStyle
  , className :: String
  , collapsible :: Boolean
  , collapsed :: Boolean
  , onToggle :: Maybe action
  }

-- | Configuration for section components
type SectionConfig =
  { title :: Maybe String
  , className :: String
  , showDivider :: Boolean
  }

-- | Configuration for card components
type CardConfig =
  { title :: Maybe String
  , subtitle :: Maybe String
  , className :: String
  , elevated :: Boolean
  }

--------------------------------------------------------------------------------
-- Default Configurations
--------------------------------------------------------------------------------

-- | Default panel configuration
defaultPanelConfig :: forall action. PanelConfig action
defaultPanelConfig =
  { title: Nothing
  , style: DefaultPanel
  , className: ""
  , collapsible: false
  , collapsed: false
  , onToggle: Nothing
  }

-- | Default section configuration
defaultSectionConfig :: SectionConfig
defaultSectionConfig =
  { title: Nothing
  , className: "section"
  , showDivider: false
  }

-- | Default card configuration
defaultCardConfig :: CardConfig
defaultCardConfig =
  { title: Nothing
  , subtitle: Nothing
  , className: "card"
  , elevated: false
  }

--------------------------------------------------------------------------------
-- Panel Components
--------------------------------------------------------------------------------

-- | Render a panel with optional title and configurable styling
renderPanel :: forall action m. PanelConfig action -> Array (H.ComponentHTML action () m) -> H.ComponentHTML action () m
renderPanel config content =
  HH.div
    [ HP.class_ (HH.ClassName (show config.style <> " " <> config.className))
    ]
    (renderPanelHeader config <> renderPanelContent config content)

-- | Render panel header with title and optional toggle
renderPanelHeader :: forall action m. PanelConfig action -> Array (H.ComponentHTML action () m)
renderPanelHeader config =
  case config.title of
    Nothing -> []
    Just title ->
      [ HH.div
          [ HP.class_ (HH.ClassName "panel__header") ]
          [ HH.h2
              [ HP.class_ (HH.ClassName "panel__title") ]
              [ HH.text title ]
          , renderToggleButton config
          ]
      ]

-- | Render panel content with optional collapse functionality
renderPanelContent :: forall action m. PanelConfig action -> Array (H.ComponentHTML action () m) -> Array (H.ComponentHTML action () m)
renderPanelContent config content =
  if config.collapsible && config.collapsed
    then []
    else
      [ HH.div
          [ HP.class_ (HH.ClassName "panel__content") ]
          content
      ]

-- | Render toggle button for collapsible panels
renderToggleButton :: forall action m. PanelConfig action -> H.ComponentHTML action () m
renderToggleButton config =
  if config.collapsible
    then case config.onToggle of
      Nothing -> HH.text ""
      Just toggleAction ->
        HH.button
          [ HP.class_ (HH.ClassName "panel__toggle")
          , HP.type_ HP.ButtonButton
          , HE.onClick \_ -> toggleAction
          ]
          [ HH.text (if config.collapsed then "▶" else "▼") ]
    else HH.text ""

-- | Render a section with optional title and divider
renderSection :: forall action m. SectionConfig -> Array (H.ComponentHTML action () m) -> H.ComponentHTML action () m
renderSection config content =
  HH.div
    [ HP.class_ (HH.ClassName config.className) ]
    (renderSectionHeader config <> content <> renderSectionDivider config)

-- | Render section header
renderSectionHeader :: forall action m. SectionConfig -> Array (H.ComponentHTML action () m)
renderSectionHeader config =
  case config.title of
    Nothing -> []
    Just title ->
      [ HH.h3
          [ HP.class_ (HH.ClassName "section__title") ]
          [ HH.text title ]
      ]

-- | Render section divider
renderSectionDivider :: forall action m. SectionConfig -> Array (H.ComponentHTML action () m)
renderSectionDivider config =
  if config.showDivider
    then [ HH.hr [ HP.class_ (HH.ClassName "section__divider") ] ]
    else []

-- | Render a card with optional title and subtitle
renderCard :: forall action m. CardConfig -> Array (H.ComponentHTML action () m) -> H.ComponentHTML action () m
renderCard config content =
  HH.div
    [ HP.class_ (HH.ClassName (config.className <> if config.elevated then " card--elevated" else "")) ]
    (renderCardHeader config <> 
     [ HH.div [ HP.class_ (HH.ClassName "card__content") ] content ])

-- | Render card header
renderCardHeader :: forall action m. CardConfig -> Array (H.ComponentHTML action () m)
renderCardHeader config =
  case config.title, config.subtitle of
    Nothing, Nothing -> []
    title, subtitle ->
      [ HH.div
          [ HP.class_ (HH.ClassName "card__header") ]
          (renderCardTitle title <> renderCardSubtitle subtitle)
      ]

-- | Render card title
renderCardTitle :: forall action m. Maybe String -> Array (H.ComponentHTML action () m)
renderCardTitle Nothing = []
renderCardTitle (Just title) =
  [ HH.h4
      [ HP.class_ (HH.ClassName "card__title") ]
      [ HH.text title ]
  ]

-- | Render card subtitle
renderCardSubtitle :: forall action m. Maybe String -> Array (H.ComponentHTML action () m)
renderCardSubtitle Nothing = []
renderCardSubtitle (Just subtitle) =
  [ HH.p
      [ HP.class_ (HH.ClassName "card__subtitle") ]
      [ HH.text subtitle ]
  ]

