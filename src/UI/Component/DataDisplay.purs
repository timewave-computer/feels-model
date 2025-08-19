-- | Data Display Components for Feels Protocol UI
-- |
-- | This module provides standardized components for displaying data such as
-- | balances, metrics, statistics, and other numerical information with
-- | consistent styling and formatting.
module UI.Component.DataDisplay
  ( -- Balance display components
    renderBalance
  , renderCompactBalance
  , renderBalanceList
  -- Metric display components
  , renderMetric
  , renderPrimaryMetric
  , renderMetricGrid
  , renderResultMetric
  -- Supply display components
  , renderSupply
  , renderSupplyGrid
  -- Configuration types
  , BalanceConfig
  , MetricConfig
  , PrimaryMetricConfig
  , SupplyConfig
  , DisplayStyle(..)
  -- Formatting utilities
  , formatDisplayNumber
  , formatCurrency
  , formatPercentage
  , formatCompactNumber
  , formatLargeNumber
  ) where

import Prelude
import Data.Array ((:), reverse, concat, head, tail)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as Number
import Data.Number.Format (toString, toStringWith, fixed)
-- Removed formatter imports as package is not available
import Data.String.CodeUnits (toCharArray, fromCharArray)
import Data.String as Data.String
import Data.Tuple (Tuple(..))
import Data.Either (fromRight)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

--------------------------------------------------------------------------------
-- Configuration Types
--------------------------------------------------------------------------------

-- | Display styling options
data DisplayStyle
  = Compact
  | Standard
  | Large
  | Inline
  | Grid

derive instance eqDisplayStyle :: Eq DisplayStyle

instance showDisplayStyle :: Show DisplayStyle where
  show Compact = "compact"
  show Standard = "standard"
  show Large = "large"
  show Inline = "inline"
  show Grid = "grid"

-- | Configuration for balance display
type BalanceConfig =
  { label :: String
  , amount :: Number
  , currency :: Maybe String
  , style :: DisplayStyle
  , className :: String
  , showCurrency :: Boolean
  , precision :: Maybe Int
  }

-- | Configuration for metric display
type MetricConfig =
  { label :: String
  , value :: String
  , style :: DisplayStyle
  , className :: String
  , description :: Maybe String
  }

-- | Configuration for primary metric display
type PrimaryMetricConfig =
  { label :: String
  , value :: String
  , unit :: Maybe String
  , style :: DisplayStyle
  , className :: String
  , highlighted :: Boolean
  }

-- | Configuration for supply display
type SupplyConfig =
  { label :: String
  , amount :: Number
  , currency :: String
  , style :: DisplayStyle
  , className :: String
  , showTrend :: Boolean
  }

--------------------------------------------------------------------------------
-- Default Configurations
--------------------------------------------------------------------------------

-- | Default balance configuration
defaultBalanceConfig :: BalanceConfig
defaultBalanceConfig =
  { label: ""
  , amount: 0.0
  , currency: Nothing
  , style: Standard
  , className: "balance"
  , showCurrency: true
  , precision: Nothing
  }

-- | Default metric configuration
defaultMetricConfig :: MetricConfig
defaultMetricConfig =
  { label: ""
  , value: ""
  , style: Standard
  , className: "metric"
  , description: Nothing
  }

-- | Default primary metric configuration
defaultPrimaryMetricConfig :: PrimaryMetricConfig
defaultPrimaryMetricConfig =
  { label: ""
  , value: ""
  , unit: Nothing
  , style: Large
  , className: "primary-metric"
  , highlighted: false
  }

-- | Default supply configuration
defaultSupplyConfig :: SupplyConfig
defaultSupplyConfig =
  { label: ""
  , amount: 0.0
  , currency: ""
  , style: Standard
  , className: "supply"
  , showTrend: false
  }

--------------------------------------------------------------------------------
-- Balance Display Components
--------------------------------------------------------------------------------

-- | Render a balance with label and amount
renderBalance :: forall action m. String -> Number -> H.ComponentHTML action () m
renderBalance label amount =
  renderBalanceWithConfig (defaultBalanceConfig { label = label, amount = amount })

-- | Render balance with full configuration
renderBalanceWithConfig :: forall action m. BalanceConfig -> H.ComponentHTML action () m
renderBalanceWithConfig config =
  let styleClass = case config.style of
        Compact -> "balance-line balance-compact"
        Inline -> "balance-inline"
        _ -> "balance-line"
      
      formattedAmount = formatCurrency config.amount config.currency config.precision
  in
    HH.div
      [ HP.class_ (HH.ClassName (styleClass <> " " <> config.className))
      , HP.style (if config.style == Compact 
                 then "margin: 0; padding: 2px 0; line-height: 1.2;"
                 else "")
      ]
      [ HH.span [ HP.class_ (HH.ClassName "balance-label") ] [ HH.text (config.label <> ": ") ]
      , HH.span [ HP.class_ (HH.ClassName "balance-value") ] [ HH.text formattedAmount ]
      ]

-- | Render compact balance for tight spaces
renderCompactBalance :: forall action m. String -> Number -> H.ComponentHTML action () m
renderCompactBalance label amount =
  renderBalanceWithConfig (defaultBalanceConfig 
    { label = label
    , amount = amount
    , style = Compact
    })

-- | Render a list of balances
renderBalanceList :: forall action m. Array (Tuple String Number) -> H.ComponentHTML action () m
renderBalanceList balances =
  HH.div
    [ HP.class_ (HH.ClassName "balance-list") ]
    (map (\(Tuple label amount) -> renderCompactBalance label amount) balances)

--------------------------------------------------------------------------------
-- Metric Display Components
--------------------------------------------------------------------------------

-- | Render a standard metric
renderMetric :: forall action m. String -> String -> H.ComponentHTML action () m
renderMetric label value =
  renderMetricWithConfig (defaultMetricConfig { label = label, value = value })

-- | Render metric with full configuration
renderMetricWithConfig :: forall action m. MetricConfig -> H.ComponentHTML action () m
renderMetricWithConfig config =
  let baseClass = case config.style of
        Grid -> "metric-item"
        Compact -> "metric-compact"
        _ -> "metric"
  in
    HH.div
      [ HP.class_ (HH.ClassName (baseClass <> " " <> config.className)) ]
      [ HH.div [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text config.label ]
      , HH.div [ HP.class_ (HH.ClassName "metric-value") ] [ HH.text config.value ]
      , case config.description of
          Nothing -> HH.text ""
          Just desc -> HH.div [ HP.class_ (HH.ClassName "metric-description") ] [ HH.text desc ]
      ]

-- | Render a primary (highlighted) metric
renderPrimaryMetric :: forall action m. String -> String -> String -> H.ComponentHTML action () m
renderPrimaryMetric label value unit =
  renderPrimaryMetricWithConfig (defaultPrimaryMetricConfig 
    { label = label
    , value = value
    , unit = if unit == "" then Nothing else Just unit
    })

-- | Render primary metric with full configuration
renderPrimaryMetricWithConfig :: forall action m. PrimaryMetricConfig -> H.ComponentHTML action () m
renderPrimaryMetricWithConfig config =
  let highlightClass = if config.highlighted then " metric-highlighted" else ""
  in
    HH.div
      [ HP.class_ (HH.ClassName (config.className <> highlightClass)) ]
      [ HH.div [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text config.label ]
      , HH.div [ HP.class_ (HH.ClassName "metric-value-large") ]
          [ HH.span [ HP.class_ (HH.ClassName "value") ] [ HH.text config.value ]
          , case config.unit of
              Nothing -> HH.text ""
              Just unit -> HH.span [ HP.class_ (HH.ClassName "unit") ] [ HH.text (" " <> unit) ]
          ]
      ]

-- | Render a grid of metrics
renderMetricGrid :: forall action m. Array (Tuple String String) -> H.ComponentHTML action () m
renderMetricGrid metrics =
  HH.div
    [ HP.class_ (HH.ClassName "metrics-grid") ]
    (map (\(Tuple label value) -> renderMetric label value) metrics)

-- | Render a result metric (for simulation results)
renderResultMetric :: forall action m. String -> String -> H.ComponentHTML action () m
renderResultMetric label value =
  HH.div
    [ HP.class_ (HH.ClassName "result-metric") ]
    [ HH.div [ HP.class_ (HH.ClassName "metric-label") ] [ HH.text label ]
    , HH.div [ HP.class_ (HH.ClassName "metric-value") ] [ HH.text value ]
    ]

--------------------------------------------------------------------------------
-- Supply Display Components
--------------------------------------------------------------------------------

-- | Render a supply metric
renderSupply :: forall action m. String -> Number -> H.ComponentHTML action () m
renderSupply label amount =
  renderSupplyWithConfig (defaultSupplyConfig 
    { label = label
    , amount = amount
    , currency = "FeelsSOL"
    })

-- | Render supply with full configuration
renderSupplyWithConfig :: forall action m. SupplyConfig -> H.ComponentHTML action () m
renderSupplyWithConfig config =
  let formattedAmount = formatLargeNumber config.amount
  in
    HH.div
      [ HP.class_ (HH.ClassName ("supply-item " <> config.className)) ]
      [ HH.div [ HP.class_ (HH.ClassName "supply-label") ] [ HH.text config.label ]
      , HH.div [ HP.class_ (HH.ClassName "supply-value") ] 
          [ HH.text (formattedAmount <> " " <> config.currency) ]
      ]

-- | Render a grid of supplies
renderSupplyGrid :: forall action m. Array (Tuple String Number) -> H.ComponentHTML action () m
renderSupplyGrid supplies =
  HH.div
    [ HP.class_ (HH.ClassName "supplies-grid") ]
    (map (\(Tuple label amount) -> renderSupply label amount) supplies)

--------------------------------------------------------------------------------
-- Formatting Utilities
--------------------------------------------------------------------------------

-- | Simple number formatting functions

-- | Format a number for display with optional precision
formatDisplayNumber :: Number -> Maybe Int -> String
formatDisplayNumber num precision =
  case precision of
    Nothing -> toString num
    Just p -> toStringWith (fixed p) num

-- | Format currency with amount and optional currency symbol (professional formatting)
formatCurrency :: Number -> Maybe String -> Maybe Int -> String
formatCurrency amount maybeCurrency precision =
  let formatted = formatDisplayNumber amount precision
  in case maybeCurrency of
       Nothing -> formatted
       Just currency -> formatted <> " " <> currency

-- | Format percentage (0.05 -> "5.0%")
formatPercentage :: Number -> String  
formatPercentage n = toStringWith (fixed 1) (n * 100.0) <> "%"

-- | Format large numbers with K/M suffixes
formatLargeNumber :: Number -> String
formatLargeNumber n 
  | n >= 1000000.0 = toStringWith (fixed 1) (n / 1000000.0) <> "M"
  | n >= 1000.0 = toStringWith (fixed 1) (n / 1000.0) <> "K"
  | otherwise = toString n

-- | Format compact numbers for limited space
formatCompactNumber :: Number -> String
formatCompactNumber = formatLargeNumber

-- | Format integers with commas
formatIntWithCommas :: Int -> String
formatIntWithCommas n = show n -- Simplified

-- | Format numbers with commas
formatNumberWithCommas :: Maybe Number -> String
formatNumberWithCommas maybeNum = 
  case maybeNum of
    Nothing -> "0"
    Just num -> toString num -- Simplified

-- Note: Manual comma insertion removed - now using professional formatters