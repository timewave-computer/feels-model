// Combined Foreign Function Interface (FFI) for all PureScript modules

//==============================================================================
// Global State Management
//==============================================================================

// Simulated global state for system tracking
let systemGlobalState = {
  totalStaked: 1000000,
  totalLiquidity: 5000000,
  utilizationRate: 0.65
};

// UI global state
let uiGlobalState = null;

// Position ID counter
let positionIdCounter = 1000;

//==============================================================================
// From Model.js - System State Functions
//==============================================================================

// Volatility matrix for token pairs
const volatilityMatrix = {
  'SOL-SOL': 0.0,
  'TokenA-SOL': 0.15,
  'TokenB-SOL': 0.20,
  'SOL-TokenA': 0.15,
  'SOL-TokenB': 0.20,
  'TokenA-TokenB': 0.25,
  'TokenB-TokenA': 0.25
};

// Helper to get volatility key from token types
function getVolatilityKey(fromToken, toToken) {
  if (fromToken === toToken) return 'SOL-SOL';
  
  const key1 = `${fromToken}-${toToken}`;
  const key2 = `${toToken}-${fromToken}`;
  
  if (volatilityMatrix[key1] !== undefined) return key1;
  if (volatilityMatrix[key2] !== undefined) return key2;
  
  return 'SOL-SOL'; // Default fallback
}

// System state exports
export const totalStaked = () => systemGlobalState.totalStaked;
export const totalLiquidity = () => systemGlobalState.totalLiquidity;
export const utilizationRate = () => systemGlobalState.utilizationRate;

export const volatility = (fromToken) => (toToken) => () => {
  const key = getVolatilityKey(fromToken, toToken);
  const vol = volatilityMatrix[key];
  if (vol !== undefined) {
    return vol;
  }
  
  // Fallback for unknown pairs - assume moderate volatility
  console.warn(`Unknown volatility pair: ${fromToken}-${toToken}, using default 0.3`);
  return 0.3;
};

//==============================================================================
// From Model.js & Position.js - Utility Functions
//==============================================================================

export const currentTime = () => Date.now();
export const generateId = (timestamp) => Math.floor(timestamp * Math.random());
export const toNumber = (intValue) => intValue;

//==============================================================================
// From UI.js - DOM Manipulation
//==============================================================================

// Set HTML content of an element
export const setInnerHTML = function(element) {
  return function(html) {
    return function() {
      element.innerHTML = html;
    };
  };
};

// Add event listener to an element
export const addEventListener = function(element) {
  return function(event) {
    return function(handler) {
      return function() {
        element.addEventListener(event, handler);
      };
    };
  };
};

// Remove all event listeners of a specific type from an element
export const removeAllEventListeners = function(element) {
  return function(event) {
    return function() {
      // Note: This doesn't actually remove all listeners,
      // but we'd need to track them to do so properly
      element.removeEventListener(event, null);
    };
  };
};

// Get value from an input element
export const getValue = function(element) {
  return function() {
    return element.value;
  };
};

// Parse string to float
export const parseFloat = (str) => globalThis.parseFloat(str);

// Math.floor
export const floor = (n) => Math.floor(n);

// Set global state (for event handlers)
export const setGlobalState = (state) => () => {
  uiGlobalState = state;
};

// Get global state
export const getGlobalState = () => uiGlobalState;

// Timeout utility
export const setTimeout = (fn) => (delay) => () => {
  window.setTimeout(fn, delay);
};

//==============================================================================
// From Main.js - Application Initialization
//==============================================================================

export const onDOMReady = function(callback) {
  return function() {
    if (document.readyState === 'loading') {
      document.addEventListener('DOMContentLoaded', callback);
    } else {
      callback();
    }
  };
};

//==============================================================================
// From TickMarket.js - Math Functions
//==============================================================================

// Base 10 logarithm
export const log10 = function(x) {
  return Math.log10(x);
};

// Generate unique position ID
export const generatePositionId = function() {
  return function() {
    return positionIdCounter++;
  };
};