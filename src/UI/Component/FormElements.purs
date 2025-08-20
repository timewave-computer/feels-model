-- | Reusable Form Elements for Feels Protocol UI
-- | 
-- | This module provides standardized form components to reduce code duplication
-- | and ensure consistent styling and behavior across the application.
module UI.Component.FormElements
  ( -- Form input components
    renderTextInput
  , renderNumberInput
  , renderPasswordInput
  , renderTextArea
  , renderSelect
  , renderButton
  , renderFormGroup
  -- Configuration types
  , TextInputConfig
  , NumberInputConfig  
  , SelectConfig
  , SelectOption
  , ButtonConfig
  , ButtonStyle(..)
  -- Default configurations
  , defaultTextInputConfig
  , defaultNumberInputConfig
  , defaultSelectConfig
  , defaultButtonConfig
  -- Helper functions
  , createSelectOption
  , createAssetSelectOptions
  , createTermTypeOptions
  , createLeverageOptions
  , createMarketScenarioOptions
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length)
import Data.String.Common (trim)
import Data.Number as Number
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Configuration for text input components
type TextInputConfig action =
  { id :: String
  , name :: String
  , placeholder :: String
  , value :: String
  , onChange :: String -> action
  , className :: String
  , label :: Maybe String
  , required :: Boolean
  , disabled :: Boolean
  }

-- | Configuration for number input components
type NumberInputConfig action =
  { id :: String
  , name :: String
  , placeholder :: String
  , value :: Number
  , onChange :: Number -> action
  , className :: String
  , label :: Maybe String
  , min :: Maybe Number
  , max :: Maybe Number
  , step :: Maybe Number
  , required :: Boolean
  , disabled :: Boolean
  }

-- | Option for select components
type SelectOption =
  { value :: String
  , label :: String
  , disabled :: Boolean
  }

-- | Configuration for select components
type SelectConfig action =
  { id :: String
  , name :: String
  , options :: Array SelectOption
  , selectedValue :: String
  , onChange :: String -> action
  , className :: String
  , label :: Maybe String
  , required :: Boolean
  , disabled :: Boolean
  }

-- | Button styling options
data ButtonStyle
  = Primary
  | Secondary
  | Success
  | Danger
  | Warning
  | Info

derive instance eqButtonStyle :: Eq ButtonStyle

instance showButtonStyle :: Show ButtonStyle where
  show Primary = "btn--primary"
  show Secondary = "btn--secondary"
  show Success = "btn--success"
  show Danger = "btn--danger"
  show Warning = "btn--warning"
  show Info = "btn--info"

-- | Configuration for button components
type ButtonConfig action =
  { text :: String
  , onClick :: action
  , style :: ButtonStyle
  , className :: String
  , disabled :: Boolean
  , type_ :: HP.ButtonType
  }

--------------------------------------------------------------------------------
-- Default Configurations
--------------------------------------------------------------------------------

-- | Default text input configuration
defaultTextInputConfig :: forall action. TextInputConfig action
defaultTextInputConfig =
  { id: ""
  , name: ""
  , placeholder: ""
  , value: ""
  , onChange: const (unsafeCrashWith "Default onChange not implemented") -- Safe default with clear error
  , className: "form__input"
  , label: Nothing
  , required: false
  , disabled: false
  }

-- | Default number input configuration
defaultNumberInputConfig :: forall action. NumberInputConfig action
defaultNumberInputConfig =
  { id: ""
  , name: ""
  , placeholder: "0"
  , value: 0.0
  , onChange: const (unsafeCrashWith "Default onChange not implemented")
  , className: "form__input"
  , label: Nothing
  , min: Nothing
  , max: Nothing
  , step: Nothing
  , required: false
  , disabled: false
  }

-- | Default select configuration
defaultSelectConfig :: forall action. SelectConfig action
defaultSelectConfig =
  { id: ""
  , name: ""
  , options: []
  , selectedValue: ""
  , onChange: const (unsafeCrashWith "Default onChange not implemented")
  , className: "form__select"
  , label: Nothing
  , required: false
  , disabled: false
  }

-- | Default button configuration
defaultButtonConfig :: forall action. ButtonConfig action
defaultButtonConfig =
  { text: ""
  , onClick: unsafeCrashWith "Default onClick not implemented"
  , style: Primary
  , className: "btn"
  , disabled: false
  , type_: HP.ButtonButton
  }

--------------------------------------------------------------------------------
-- Form Input Components
--------------------------------------------------------------------------------

-- | Render a text input with optional label
renderTextInput :: forall action m. TextInputConfig action -> H.ComponentHTML action () m
renderTextInput config =
  renderFormGroup config.label $
    HH.input
      [ HP.type_ HP.InputText
      , HP.id config.id
      , HP.name config.name
      , HP.placeholder config.placeholder
      , HP.class_ (HH.ClassName config.className)
      , HP.value config.value
      , HP.required config.required
      , HP.disabled config.disabled
      , HE.onValueChange \v -> config.onChange (trim v)
      ]

-- | Render a number input with optional label and constraints
renderNumberInput :: forall action m. NumberInputConfig action -> H.ComponentHTML action () m
renderNumberInput config =
  renderFormGroup config.label $
    HH.input
      [ HP.type_ HP.InputNumber
      , HP.id config.id
      , HP.name config.name
      , HP.placeholder config.placeholder
      , HP.class_ (HH.ClassName config.className)
      , HP.value (show config.value)
      , HP.required config.required
      , HP.disabled config.disabled
      , case config.min of
          Nothing -> HP.attr (HH.AttrName "min") ""
          Just minVal -> HP.min minVal
      , case config.max of
          Nothing -> HP.attr (HH.AttrName "max") ""
          Just maxVal -> HP.max maxVal
      , case config.step of
          Nothing -> HP.attr (HH.AttrName "step") ""
          Just stepVal -> HP.attr (HH.AttrName "step") (show stepVal)
      , HE.onValueChange \v -> config.onChange (fromMaybe 0.0 (Number.fromString v))
      ]

-- | Render a password input with optional label
renderPasswordInput :: forall action m. TextInputConfig action -> H.ComponentHTML action () m
renderPasswordInput config =
  renderFormGroup config.label $
    HH.input
      [ HP.type_ HP.InputPassword
      , HP.id config.id
      , HP.name config.name
      , HP.placeholder config.placeholder
      , HP.class_ (HH.ClassName config.className)
      , HP.value config.value
      , HP.required config.required
      , HP.disabled config.disabled
      , HE.onValueChange \v -> config.onChange (trim v)
      ]

-- | Render a textarea with optional label
renderTextArea :: forall action m. Int -> Int -> TextInputConfig action -> H.ComponentHTML action () m
renderTextArea rows cols config =
  renderFormGroup config.label $
    HH.textarea
      [ HP.id config.id
      , HP.name config.name
      , HP.placeholder config.placeholder
      , HP.class_ (HH.ClassName config.className)
      , HP.value config.value
      , HP.required config.required
      , HP.disabled config.disabled
      , HP.rows rows
      , HP.cols cols
      , HE.onValueChange \v -> config.onChange (trim v)
      ]

-- | Render a select dropdown with optional label
renderSelect :: forall action m. SelectConfig action -> H.ComponentHTML action () m
renderSelect config =
  renderFormGroup config.label $
    HH.select
      [ HP.id config.id
      , HP.name config.name
      , HP.class_ (HH.ClassName config.className)
      , HP.required config.required
      , HP.disabled config.disabled
      , HE.onValueChange config.onChange
      ]
      (renderOptions config.options config.selectedValue)
  where
    renderOptions :: Array SelectOption -> String -> Array (H.ComponentHTML action () m)
    renderOptions options selectedValue =
      map renderOption options
      where
        renderOption :: SelectOption -> H.ComponentHTML action () m
        renderOption option =
          HH.option
            [ HP.value option.value
            , HP.disabled option.disabled
            , HP.selected (option.value == selectedValue)
            ]
            [ HH.text option.label ]

-- | Render a button with standardized styling
renderButton :: forall action m. ButtonConfig action -> H.ComponentHTML action () m
renderButton config =
  HH.button
    [ HE.onClick \_ -> config.onClick
    , HP.class_ (HH.ClassName (config.className <> " " <> show config.style))
    , HP.disabled config.disabled
    , HP.type_ config.type_
    ]
    [ HH.text config.text ]

--------------------------------------------------------------------------------
-- Form Structure Components
--------------------------------------------------------------------------------

-- | Render a form group with optional label
renderFormGroup :: forall action m. Maybe String -> H.ComponentHTML action () m -> H.ComponentHTML action () m
renderFormGroup maybeLabel input =
  case maybeLabel of
    Nothing -> input
    Just labelText ->
      HH.div
        [ HP.class_ (HH.ClassName "form__group") ]
        [ HH.label [] [ HH.text labelText ]
        , input
        ]

-- | Render validation errors
renderValidationErrors :: forall action m. Array String -> H.ComponentHTML action () m
renderValidationErrors errors =
  if length errors == 0
    then HH.text ""
    else
      HH.div
        [ HP.class_ (HH.ClassName "form__errors") ]
        (map renderError errors)
  where
    renderError :: String -> H.ComponentHTML action () m
    renderError error =
      HH.div
        [ HP.class_ (HH.ClassName "form__error") ]
        [ HH.text error ]

--------------------------------------------------------------------------------
-- Helper Functions for Common Select Options
--------------------------------------------------------------------------------

-- | Create a select option
createSelectOption :: String -> String -> Boolean -> SelectOption
createSelectOption value label disabled =
  { value: value
  , label: label
  , disabled: disabled
  }

-- | Create options for asset selection (from/to assets)
createAssetSelectOptions :: Array SelectOption
createAssetSelectOptions =
  [ createSelectOption "jitosol" "JitoSOL" false
  , createSelectOption "feelssol" "FeelsSOL" false
  , createSelectOption "position" "Existing Position" false
  , createSelectOption "position-swap" "Swap Position" false
  , createSelectOption "position-term" "Term Position" false
  ]

-- | Create options for term type selection
createTermTypeOptions :: Array SelectOption
createTermTypeOptions =
  [ createSelectOption "swap" "Swap (Flexible)" false
  , createSelectOption "monthly" "Monthly Term (Join Current Cycle)" false
  ]

-- | Create options for leverage/risk level selection
createLeverageOptions :: Array SelectOption
createLeverageOptions =
  [ createSelectOption "senior" "Senior (1x - Protected)" false
  , createSelectOption "junior" "Junior (3x - Higher Risk/Reward)" false
  ]

-- | Create options for market scenarios
createMarketScenarioOptions :: Array SelectOption
createMarketScenarioOptions =
  [ createSelectOption "BullMarket" "Bull Market" false
  , createSelectOption "BearMarket" "Bear Market" false
  , createSelectOption "SidewaysMarket" "Sideways Market" false
  , createSelectOption "VolatileMarket" "Volatile Market" false
  ]

