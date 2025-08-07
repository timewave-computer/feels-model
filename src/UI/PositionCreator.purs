module UI.PositionCreator
  ( PositionCreatorState
  , PositionCreatorAction(..)
  , initPositionCreator
  , updatePositionCreator
  , renderBandSelector
  , renderTermSelector
  , renderLeverageSlider
  , renderAdvancedOptions
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Position
import Token (TokenType(..))
import RiskManagement (HealthFactor)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- Position creator state
type PositionCreatorState =
  { -- Basic inputs
    amount :: Number
  , tokenPair :: TokenPair
  
  -- Band selection
  , selectedBand :: BandTier
  , useAdvancedMode :: Boolean
  , customTickRange :: Maybe TickRange
  
  -- Term selection
  , selectedTerm :: TermCommitment
  
  -- Leverage selection
  , targetLeverage :: Number
  , useDynamicLeverage :: Boolean
  , currentHealthFactor :: HealthFactor
  , maxAllowedLeverage :: Number
  
  -- UI state
  , showAdvanced :: Boolean
  , validationErrors :: Array String
  }

-- Actions for updating state
data PositionCreatorAction
  = SetAmount Number
  | SetTokenPair TokenPair
  | SelectBand BandTier
  | ToggleAdvancedMode
  | SetCustomRange (Maybe TickRange)
  | SelectTerm TermCommitment
  | SetLeverage Number
  | ToggleDynamicLeverage
  | ToggleAdvancedOptions
  | ValidateInputs

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

-- Initialize position creator
initPositionCreator :: Effect PositionCreatorState
initPositionCreator = pure
  { amount: 0.0
  , tokenPair: { base: FeelsSOL, quote: JitoSOL }
  , selectedBand: MediumBand
  , useAdvancedMode: false
  , customTickRange: Nothing
  , selectedTerm: Spot
  , targetLeverage: 1.0
  , useDynamicLeverage: false
  , currentHealthFactor: 1.5
  , maxAllowedLeverage: 10.0
  , showAdvanced: false
  , validationErrors: []
  }

--------------------------------------------------------------------------------
-- State Updates
--------------------------------------------------------------------------------

-- Update position creator state
updatePositionCreator :: PositionCreatorAction -> PositionCreatorState -> PositionCreatorState
updatePositionCreator action state = case action of
  SetAmount amount -> 
    state { amount = amount }
  
  SetTokenPair pair -> 
    state { tokenPair = pair }
  
  SelectBand band -> 
    state { selectedBand = band }
  
  ToggleAdvancedMode ->
    state { useAdvancedMode = not state.useAdvancedMode }
  
  SetCustomRange range ->
    state { customTickRange = range }
  
  SelectTerm term ->
    state { selectedTerm = term }
  
  SetLeverage lev ->
    let clampedLev = max 1.0 (min state.maxAllowedLeverage lev)
    in state { targetLeverage = clampedLev }
  
  ToggleDynamicLeverage ->
    state { useDynamicLeverage = not state.useDynamicLeverage }
  
  ToggleAdvancedOptions ->
    state { showAdvanced = not state.showAdvanced }
  
  ValidateInputs ->
    let errors = validatePosition state
    in state { validationErrors = errors }

--------------------------------------------------------------------------------
-- UI Components
--------------------------------------------------------------------------------

-- Render band selection interface
renderBandSelector :: PositionCreatorState -> String
renderBandSelector state = 
  """
  <div class="band-selector">
    <h3>Select Price Band</h3>
    <div class="band-options">
      """ <> renderBandOption TightBand state <> """
      """ <> renderBandOption MediumBand state <> """
      """ <> renderBandOption WideBand state <> """
    </div>
    """ <> if state.useAdvancedMode 
           then """
    <div class="advanced-options">
      <label>Custom Slippage Tolerance: 
        <input type="number" min="0.001" max="0.1" step="0.001" value="0.01" />
      </label>
    </div>
           """
           else "" <> """
  </div>
  """

-- Render individual band option
renderBandOption :: BandTier -> PositionCreatorState -> String
renderBandOption band state =
  let isSelected = band == state.selectedBand
      bandInfo = case band of
        TightBand -> Tuple "Tight" (Tuple "±1%" "Lowest fees, active management")
        MediumBand -> Tuple "Medium" (Tuple "±5%" "Balanced approach") 
        WideBand -> Tuple "Wide" (Tuple "±10%" "Set and forget")
      Tuple name (Tuple range desc) = bandInfo
  in """
  <div class="band-option """ <> if isSelected then "selected" else "" <> """">
    <h4>""" <> name <> """ Band</h4>
    <div class="band-range">""" <> range <> """</div>
    <div class="band-description">""" <> desc <> """</div>
    <div class="fee-discount">10% fee discount</div>
  </div>
  """

-- Render term selection interface
renderTermSelector :: PositionCreatorState -> String
renderTermSelector state =
  """
  <div class="term-selector">
    <h3>Select Term Commitment</h3>
    <div class="term-options">
      """ <> renderTermOption Spot state <> """
      """ <> renderTermOption (Hourly 0.0) state <> """
      """ <> renderTermOption (Daily 0.0) state <> """
      """ <> renderTermOption (Weekly 0.0) state <> """
    </div>
    <div class="term-info">
      """ <> renderTermInfo state.selectedTerm <> """
    </div>
  </div>
  """

-- Render term option
renderTermOption :: TermCommitment -> PositionCreatorState -> String
renderTermOption term state =
  let isSelected = termEquals term state.selectedTerm
      Tuple name desc = case term of
        Spot -> Tuple "Spot" "No commitment"
        Hourly _ -> Tuple "Hourly" "Next hour"
        Daily _ -> Tuple "Daily" "Next midnight"
        Weekly _ -> Tuple "Weekly" "Next Sunday"
  in """
  <div class="term-option """ <> if isSelected then "selected" else "" <> """">
    <h4>""" <> name <> """</h4>
    <div class="term-desc">""" <> desc <> """</div>
  </div>
  """

-- Render leverage slider interface
renderLeverageSlider :: PositionCreatorState -> String
renderLeverageSlider state =
  let healthAdjusted = adjustMaxLeverageForHealth state.currentHealthFactor state.maxAllowedLeverage
      effectiveMax = min healthAdjusted state.maxAllowedLeverage
  in """
  <div class="leverage-selector">
    <h3>Select Leverage</h3>
    <div class="leverage-slider">
      <input type="range" 
             min="1" 
             max='""" <> show effectiveMax <> """'
             step="0.1"
             value='""" <> show state.targetLeverage <> """'>
      <div class="leverage-display">""" <> show state.targetLeverage <> """x</div>
    </div>
    
    <div class="leverage-mode">
      <label>
        <input type="checkbox" """ <> 
        if state.useDynamicLeverage then "checked" else "" <> """>
        Dynamic leverage adjustment
      </label>
      <div class="mode-description">
        """ <> if state.useDynamicLeverage 
               then "Leverage will adjust based on pool health"
               else "Leverage remains fixed" <> """
      </div>
    </div>
    
    """ <> if state.currentHealthFactor < 1.5
           then renderHealthWarning state.currentHealthFactor
           else "" <> """
  </div>
  """

-- Render advanced options interface
renderAdvancedOptions :: PositionCreatorState -> String
renderAdvancedOptions state =
  if not state.showAdvanced
    then """
    <button class="show-advanced">Show Advanced Options</button>
    """
    else """
    <div class="advanced-options">
      <h3>Advanced Options</h3>
      
      <div class="price-strategy">
        <h4>Price Strategy</h4>
        <select>
          <option value="band">Band Aligned (10% discount)</option>
          <option value="tick">Tick Specific (no discount)</option>
          <option value="hybrid">Band Constrained (5% discount)</option>
        </select>
      </div>
      
      """ <> if state.useAdvancedMode
             then renderTickRangeInputs state
             else "" <> """
      
      <div class="tracking-mode">
        <h4>Price Tracking</h4>
        <select>
          <option value="spot">Spot Price</option>
          <option value="twap">TWAP (15 min)</option>
          <option value="mid">Bid/Ask Midpoint</option>
        </select>
      </div>
      
      <button class="hide-advanced">Hide Advanced Options</button>
    </div>
    """

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- Validate position inputs
validatePosition :: PositionCreatorState -> Array String
validatePosition state =
  let amountErrors = if state.amount <= 0.0 
                     then ["Amount must be positive"] 
                     else []
      leverageErrors = if state.targetLeverage < 1.0 || state.targetLeverage > 10.0
                       then ["Leverage must be between 1x and 10x"]
                       else []
      rangeErrors = case state.customTickRange of
        Just range -> if range.upperTick <= range.lowerTick
                      then ["Upper tick must be greater than lower tick"]
                      else []
        Nothing -> []
  in amountErrors <> leverageErrors <> rangeErrors

-- Check if terms are equal
termEquals :: TermCommitment -> TermCommitment -> Boolean
termEquals Spot Spot = true
termEquals (Hourly _) (Hourly _) = true
termEquals (Daily _) (Daily _) = true
termEquals (Weekly _) (Weekly _) = true
termEquals _ _ = false

-- Render term information
renderTermInfo :: TermCommitment -> String
renderTermInfo term = case term of
  Spot -> "Perpetual position with no expiry. Withdraw anytime."
  Hourly _ -> "Expires at the next hour. Automatically converts to spot."
  Daily _ -> "Expires at midnight UTC. Earns daily term rates."
  Weekly _ -> "Expires Sunday midnight UTC. Best rates for commitment."

-- Render tick range inputs
renderTickRangeInputs :: PositionCreatorState -> String
renderTickRangeInputs state =
  """
  <div class="tick-range-inputs">
    <h4>Custom Price Range</h4>
    <div class="range-input">
      <label>Lower Price</label>
      <input type="number" placeholder="0.95" step="0.01">
    </div>
    <div class="range-input">
      <label>Upper Price</label>
      <input type="number" placeholder="1.05" step="0.01">
    </div>
  </div>
  """

-- Render health warning
renderHealthWarning :: HealthFactor -> String
renderHealthWarning health =
  """
  <div class="health-warning">
    <strong>Warning:</strong> Pool health is """ <> show health <> """.
    Maximum leverage has been reduced for safety.
  </div>
  """

-- Adjust max leverage based on health
adjustMaxLeverageForHealth :: HealthFactor -> Number -> Number
adjustMaxLeverageForHealth health maxLev =
  if health >= 1.5 then maxLev
  else if health >= 1.2 then maxLev * 0.7
  else if health >= 1.0 then maxLev * 0.5
  else maxLev * 0.2