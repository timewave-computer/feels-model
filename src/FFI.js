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

// Math functions
export const sqrt = (x) => Math.sqrt(x);

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
  console.log('setTimeout called with delay:', delay);
  return window.setTimeout(() => {
    console.log('setTimeout executing callback');
    const result = fn();
    console.log('Callback result type:', typeof result);
    if (typeof result === 'function') {
      console.log('Result is a function, executing it');
      result();
    }
  }, delay);
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

// Generate unique record ID (for lending records)
export const generateRecordId = function() {
  return function() {
    return positionIdCounter++;
  };
};

// Get attribute value from an element
export const getAttribute = function(attrName) {
  return function(element) {
    return function() {
      return element.getAttribute(attrName) || "";
    };
  };
};

//==============================================================================
// Remote Control Functions
//==============================================================================

// Register a remote control action
export const registerRemoteAction = function(name) {
  return function(handler) {
    return function() {
      if (window.remoteControl && window.remoteControl.registerAction) {
        // Wrap the PureScript handler to work with JavaScript promises
        window.remoteControl.registerAction(name, function(params) {
          return new Promise((resolve, reject) => {
            try {
              // Execute the PureScript handler
              const result = handler(params)();
              resolve(result);
            } catch (e) {
              reject(e);
            }
          });
        });
        console.log(`Registered remote action: ${name}`);
      } else {
        console.warn('Remote control not available');
      }
    };
  };
};

// Trigger a UI action by simulating a button click
export const triggerUIAction = function(actionName) {
  return function() {
    console.log(`Triggering UI action: ${actionName}`);
    
    // Map action names to button selectors
    // Handle special action for testing token validation
    if (actionName === 'testTokenValidation') {
      console.log('Testing token validation with scdf/sfd values...');
      
      // Set the input values directly
      const tickerInput = document.querySelector('#token-ticker');
      const nameInput = document.querySelector('#token-name');
      
      if (tickerInput && nameInput) {
        // Simulate typing to trigger validation
        tickerInput.value = 'scdf';
        nameInput.value = 'sfd';
        
        // Trigger input events to trigger validation
        tickerInput.dispatchEvent(new Event('input', { bubbles: true }));
        nameInput.dispatchEvent(new Event('input', { bubbles: true }));
        
        // Also try 'change' events for Halogen compatibility
        tickerInput.dispatchEvent(new Event('change', { bubbles: true }));
        nameInput.dispatchEvent(new Event('change', { bubbles: true }));
        
        console.log('Token validation test values set and events dispatched');
        return true;
      } else {
        console.error('Token input fields not found');
        return false;
      }
    }
    
    const actionMap = {
      'runSimulation': 'button#run-simulation-btn',
      'refreshData': 'button#refresh-btn',
      'enterGateway': 'button#gateway-btn'
    };
    
    const selector = actionMap[actionName];
    if (selector) {
      const button = document.querySelector(selector);
      if (button) {
        console.log(`Found button for ${actionName}, clicking...`);
        button.click();
        return true;
      } else {
        console.error(`Button not found for action ${actionName} (selector: ${selector})`);
        return false;
      }
    } else {
      console.error(`Unknown UI action: ${actionName}`);
      return false;
    }
  };
};

// Execute a remote action (for testing)
export const executeRemoteAction = function(name) {
  return function(params) {
    return function() {
      if (window.remoteControl && window.remoteControl.executeAction) {
        return window.remoteControl.executeAction(name, params);
      } else {
        console.error('Remote control not available');
        return null;
      }
    };
  };
};

//==============================================================================
// Chart Rendering Functions
//==============================================================================

// Global chart instance
let chartInstance = null;

// Check if chart DOM elements are ready and initialize
export const checkAndInitializeChart = function() {
  console.log('checkAndInitializeChart wrapper called');
  return function() {
    console.log('=== CHECKING CHART READINESS ===');
    console.log('Document ready state:', document.readyState);
    
    // Check if Chart.js is loaded
    if (typeof Chart === 'undefined') {
      console.error('Chart.js is not loaded! Retrying in 500ms...');
      window.setTimeout(() => checkAndInitializeChart()(), 500);
      return;
    }
    console.log('Chart.js version:', Chart.version);
    
    // Chart.js is loaded and ready - using standard line chart
    
    const canvas = document.getElementById('price-chart');
    const dataElement = document.querySelector('.chart-data');
    
    console.log('Canvas found:', !!canvas);
    console.log('Data element found:', !!dataElement);
    
    if (!canvas) {
      console.error('Canvas element not found! Looking for id="price-chart"');
      console.log('All canvas elements:', document.querySelectorAll('canvas').length);
    }
    
    if (!dataElement) {
      console.error('Data element not found! Looking for class="chart-data"');
      console.log('All elements with chart-data class:', document.querySelectorAll('.chart-data').length);
      // Try to find any element that might contain chart data
      const panelElements = document.querySelectorAll('.panel');
      console.log('Number of panel elements:', panelElements.length);
      panelElements.forEach((panel, i) => {
        if (panel.textContent.includes('Price History')) {
          console.log(`Panel ${i} contains Price History section`);
          console.log('Panel HTML:', panel.innerHTML.substring(0, 300));
        }
      });
    }
    
    if (!canvas || !dataElement) {
      console.warn('Chart elements not ready, retrying in 250ms...');
      window.setTimeout(() => checkAndInitializeChart()(), 250);
      return;
    }
    
    // Check canvas dimensions
    if (canvas.width === 0 || canvas.height === 0) {
      console.warn('Canvas has zero dimensions, retrying in 250ms...');
      window.setTimeout(() => checkAndInitializeChart()(), 250);
      return;
    }
    
    const rawData = dataElement.textContent;
    console.log('Data element content length:', rawData?.length || 0);
    console.log('Data element content preview:', rawData?.substring(0, 100));
    
    if (!rawData || rawData.trim() === '' || rawData === '[]') {
      console.warn('Chart data not ready or empty, retrying in 250ms...');
      window.setTimeout(() => checkAndInitializeChart()(), 250);
      return;
    }
    
    console.log('Chart elements ready, initializing chart...');
    initializePriceChart()();
  };
};

// Initialize price chart with NFV overlay using Chart.js
export const initializePriceChart = function() {
  return function() {
    console.log('=== CHART INITIALIZATION STARTING ===');
    const canvas = document.getElementById('price-chart');
    const dataElement = document.querySelector('.chart-data');
    
    console.log('Canvas found:', !!canvas);
    console.log('Canvas ID:', canvas?.id);
    console.log('Canvas parent:', canvas?.parentElement?.className);
    console.log('Data element found:', !!dataElement);
    console.log('Data element class:', dataElement?.className);
    console.log('Canvas dimensions:', canvas?.width, 'x', canvas?.height);
    console.log('Canvas computed style:', canvas ? window.getComputedStyle(canvas).display : 'no canvas');
    
    // Check if Chart.js is loaded
    if (typeof Chart === 'undefined') {
      console.error('Chart.js is not loaded!');
      return;
    }
    console.log('Chart.js version:', Chart.version);
    
    if (!canvas || !dataElement) {
      console.warn('Chart canvas or data not found');
      return;
    }

    try {
      const rawData = dataElement.textContent;
      console.log('Raw chart data length:', rawData?.length || 0);
      console.log('Raw chart data preview:', rawData?.substring(0, 200) || 'empty');
      console.log('Raw chart data full:', rawData);
      
      if (!rawData || rawData.trim() === '' || rawData === '[]') {
        console.log('No chart data available yet');
        console.log('Drawing placeholder on canvas...');
        const ctx = canvas.getContext('2d');
        ctx.fillStyle = '#1a1a1a';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.fillStyle = '#666';
        ctx.font = '14px sans-serif';
        ctx.fillText('No data - run simulation first', 10, canvas.height/2);
        console.log('Placeholder drawn');
        return;
      }
      
      const data = JSON.parse(rawData);
      console.log('Parsed chart data:', data);
      console.log('Data points count:', data.length);
      console.log('First data point:', data[0]);
      console.log('Last data point:', data[data.length - 1]);
      
      if (!Array.isArray(data) || data.length === 0) {
        console.log('Empty data array');
        return;
      }
      
      console.log('=== CALLING RENDER CHART ===');
      renderChartJS(canvas, data);
      console.log('=== CHART RENDER COMPLETE ===');
    } catch (e) {
      console.error('Failed to parse chart data:', e);
      console.error('Error stack:', e.stack);
      const ctx = canvas.getContext('2d');
      ctx.fillStyle = 'red';
      ctx.fillText('Chart Error: ' + e.message, 10, 20);
    }
  };
};

// Simple canvas-based chart renderer
function renderChart(canvas, data) {
  console.log('renderChart called with canvas:', canvas, 'data:', data);
  
  if (!canvas) {
    console.error('Canvas is null or undefined');
    return;
  }
  
  const ctx = canvas.getContext('2d');
  if (!ctx) {
    console.error('Could not get 2D context from canvas');
    return;
  }
  
  const width = canvas.width;
  const height = canvas.height;
  
  console.log('Starting chart render, canvas size:', width, 'x', height);
  console.log('Data points:', data ? data.length : 'data is null/undefined');
  console.log('Data sample:', data ? data.slice(0, 3) : 'no data');
  
  // Clear canvas with a visible test color first
  ctx.fillStyle = '#ff0000'; // Red background to test canvas
  ctx.fillRect(0, 0, width, height);
  
  // Wait a moment then draw actual background
  setTimeout(() => {
    ctx.fillStyle = '#0a0a0a';
    ctx.fillRect(0, 0, width, height);
    
    if (!data || data.length === 0) {
      console.log('No data to render, drawing placeholder');
      ctx.fillStyle = '#ffffff';
      ctx.font = '16px sans-serif';
      ctx.fillText('No data available', width/2 - 60, height/2);
      return;
    }
    
    // Calculate scales
    const priceValues = data.map(d => d.price);
    const nfvValues = data.map(d => d.nfvValue);
    
    const minPrice = Math.min(...priceValues);
    const maxPrice = Math.max(...priceValues);
    const minNFV = Math.min(...nfvValues);
    const maxNFV = Math.max(...nfvValues);
    
    console.log('Price range:', minPrice, 'to', maxPrice);
    console.log('NFV range:', minNFV, 'to', maxNFV);
    
    const margin = 40;
    const chartWidth = width - 2 * margin;
    const chartHeight = height - 2 * margin;
    
    console.log('Chart area:', chartWidth, 'x', chartHeight);
    
    // Helper functions
    const scaleX = (i) => margin + (i / Math.max(1, data.length - 1)) * chartWidth;
    const scalePriceY = (price) => {
      if (maxPrice === minPrice) return height / 2;
      return margin + (1 - (price - minPrice) / (maxPrice - minPrice)) * chartHeight;
    };
    const scaleNFVY = (nfv) => {
      if (maxNFV === minNFV) return height / 2;
      return margin + (1 - (nfv - minNFV) / (maxNFV - minNFV)) * chartHeight;
    };
    
    // Draw axes
    ctx.strokeStyle = '#666';
    ctx.lineWidth = 1;
    ctx.beginPath();
    ctx.moveTo(margin, margin);
    ctx.lineTo(margin, height - margin);
    ctx.lineTo(width - margin, height - margin);
    ctx.stroke();
    console.log('Drew axes');
    
    // Draw price line (pink)
    console.log('Drawing price line...');
    ctx.strokeStyle = '#ffc0cb';
    ctx.lineWidth = 3;
    ctx.beginPath();
    
    let validPricePoints = 0;
    data.forEach((point, i) => {
      const x = scaleX(i);
      const y = scalePriceY(point.price);
      console.log(`Price point ${i}: x=${x}, y=${y}, price=${point.price}`);
      
      if (isFinite(x) && isFinite(y)) {
        if (validPricePoints === 0) {
          ctx.moveTo(x, y);
        } else {
          ctx.lineTo(x, y);
        }
        validPricePoints++;
      }
    });
    
    if (validPricePoints > 0) {
      ctx.stroke();
      console.log(`Drew price line with ${validPricePoints} points`);
    } else {
      console.log('No valid price points to draw');
    }
    
    // Draw NFV line (pink)
    console.log('Drawing NFV line...');
    ctx.strokeStyle = '#ff91a4';
    ctx.lineWidth = 3;
    ctx.beginPath();
    
    let validNFVPoints = 0;
    data.forEach((point, i) => {
      const x = scaleX(i);
      const y = scaleNFVY(point.nfvValue);
      console.log(`NFV point ${i}: x=${x}, y=${y}, nfv=${point.nfvValue}`);
      
      if (isFinite(x) && isFinite(y)) {
        if (validNFVPoints === 0) {
          ctx.moveTo(x, y);
        } else {
          ctx.lineTo(x, y);
        }
        validNFVPoints++;
      }
    });
    
    if (validNFVPoints > 0) {
      ctx.stroke();
      console.log(`Drew NFV line with ${validNFVPoints} points`);
    } else {
      console.log('No valid NFV points to draw');
    }
    
    // Add labels
    ctx.fillStyle = '#e0e0e0';
    ctx.font = '14px sans-serif';
    ctx.fillText(`Price: ${(minPrice != null ? minPrice.toFixed(3) : '0.000')} - ${(maxPrice != null ? maxPrice.toFixed(3) : '0.000')}`, margin, 25);
    ctx.fillText(`NFV: ${(minNFV != null ? minNFV.toFixed(3) : '0.000')} - ${(maxNFV != null ? maxNFV.toFixed(3) : '0.000')}`, margin + 200, 25);
    
    // Add title
    ctx.fillStyle = '#ffffff';
    ctx.font = 'bold 16px sans-serif';
    ctx.fillText('Price & NFV Chart', margin, height - 10);
    
    console.log('Chart rendering complete');
  }, 50); // Small delay to ensure canvas is ready
}

//==============================================================================
// Selective DOM Update Functions
//==============================================================================

// Get element by ID
export const getElementById = function(elementId) {
  return function() {
    return document.getElementById(elementId);
  };
};

// Chart.js rendering function
function renderChartJS(canvas, data) {
  console.log('renderChartJS called with data:', data);
  console.log('Data length:', data.length);
  console.log('First data point:', data[0]);
  console.log('Canvas element:', canvas);
  console.log('Canvas width:', canvas.width, 'height:', canvas.height);
  
  // Destroy existing chart if it exists
  if (chartInstance) {
    console.log('Destroying existing chart instance');
    chartInstance.destroy();
    chartInstance = null;
  }
  
  const ctx = canvas.getContext('2d');
  console.log('Got 2D context:', !!ctx);
  
  // Check if data has multiple tokens or single price format
  const isMultiToken = data.length > 0 && data[0].tokens;
  console.log('Is multi-token data:', isMultiToken);
  console.log('First data point:', data[0]);
  
  if (isMultiToken) {
    console.log('Calling renderMultiTokenChart');
    renderMultiTokenChart(ctx, data);
  } else {
    console.log('Calling renderSingleTokenChart');
    renderSingleTokenChart(ctx, data);
  }
}

// Render chart for single token (JitoSOL/FeelsSOL)
function renderSingleTokenChart(ctx, data) {
  console.log('renderSingleTokenChart called with', data.length, 'data points');
  
  try {
    // Filter out invalid data points
    console.log('Filtering data. First point:', data[0]);
    const validData = data.filter(point => {
      const isValid = point && 
        (point.block != null || point.timestamp) && 
        point.price != null && 
        !isNaN(point.price) && 
        isFinite(point.price);
      
      if (!isValid) {
        console.log('Invalid point:', point);
      }
      return isValid;
    });
    
    if (validData.length === 0) {
      console.error('No valid data points to chart');
      return;
    }
    
    // Convert data to Chart.js format for line charts
    const priceData = validData.map(point => ({
      x: Number(point.block) || 0,  // Use block number for x-axis
      y: Number(point.price)
    }));
    
    console.log('Price data sample:', priceData.slice(0, 3));
  
  // NFV floor data as a line
  const nfvData = validData.map(point => ({
    x: Number(point.block) || 0,  // Use block number for x-axis
    y: point.nfvValue != null && !isNaN(point.nfvValue) && isFinite(point.nfvValue) ? Number(point.nfvValue) : 0
  }));
  
  // Create the chart
  try {
    console.log('About to create Chart.js instance...');
    console.log('Chart constructor available:', typeof Chart);
    console.log('priceData length:', priceData.length);
    console.log('First few priceData points:', priceData.slice(0, 3));
    console.log('nfvData length:', nfvData.length);
    console.log('First few nfvData points:', nfvData.slice(0, 3));
    
    // Always use line chart - removed financial plugin dependency
    const chartType = 'line';
    
    console.log('Chart data ready:', priceData.slice(0, 3));
    
    console.log('Creating Chart instance with type:', chartType);
    console.log('Chart data points:', priceData.length);
    console.log('NFV data points:', nfvData.length);
    
    chartInstance = new Chart(ctx, {
      type: chartType,
      data: {
        datasets: [{
          label: 'JitoSOL/FeelsSOL Price',
          data: priceData,
          borderColor: '#ffc0cb',
          backgroundColor: 'rgba(255, 192, 203, 0.1)',
          borderWidth: 2,
          pointRadius: 1,
          tension: 0.1,
          fill: false
        }, {
          label: 'NFV Floor',
          type: 'line',
          data: nfvData,
          borderColor: '#f59e0b',
          backgroundColor: 'rgba(245, 158, 11, 0.1)',
          borderWidth: 2,
          pointRadius: 0,
          fill: true,
          yAxisID: 'y2'
        }]
      },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      plugins: {
        legend: {
          display: true,
          labels: {
            color: '#000000',
            font: {
              family: 'monospace'
            }
          }
        },
        tooltip: {
          mode: 'index',
          intersect: false,
          backgroundColor: 'rgba(0, 0, 0, 0.8)',
          titleColor: '#e5e7eb',
          bodyColor: '#e5e7eb',
          borderColor: '#374151',
          borderWidth: 1,
          callbacks: {
            label: function(context) {
              if (!context.parsed || context.parsed.y == null) {
                return 'No data';
              }
              return `${context.dataset.label}: ${context.parsed.y.toFixed(4)}`;
            }
          }
        }
      },
      scales: {
        x: {
          type: 'linear',
          title: {
            display: true,
            text: 'Block Number',
            color: '#000000',
            font: {
              family: 'monospace'
            }
          },
          min: validData.length > 0 ? Math.min(...validData.map(d => d.block || 0)) : 0,
          max: validData.length > 0 ? Math.max(...validData.map(d => d.block || 0)) : 100,
          grid: {
            color: '#1f2937',
            drawBorder: false
          },
          ticks: {
            color: '#000000',
            font: {
              family: 'monospace'
            },
            autoSkip: true,
            maxTicksLimit: 10
          }
        },
        y: {
          position: 'left',
          grid: {
            color: '#1f2937',
            drawBorder: false
          },
          ticks: {
            color: '#000000',
            font: {
              family: 'monospace'
            },
            callback: function(value) {
              return (value != null && typeof value === 'number') ? value.toFixed(3) : '0.000';
            }
          },
          title: {
            display: true,
            text: 'Price (JitoSOL/FeelsSOL)',
            color: '#000000'
          }
        },
        y2: {
          position: 'right',
          grid: {
            drawOnChartArea: false
          },
          ticks: {
            color: '#f59e0b',
            font: {
              family: 'monospace'
            },
            callback: function(value) {
              return (value != null && typeof value === 'number') ? value.toFixed(3) : '0.000';
            }
          },
          title: {
            display: true,
            text: 'NFV Floor',
            color: '#f59e0b'
          }
        }
      }
    }
  });
  
  console.log('Chart.js chart created successfully');
  console.log('Chart instance available:', !!chartInstance);
  console.log('Chart instance ID:', chartInstance?.id);
  console.log('Chart datasets:', chartInstance?.data?.datasets?.length);
  
  // Make chart instance globally available for debugging
  window.currentChartInstance = chartInstance;
  
  } catch (error) {
    console.error('Error creating Chart.js chart:', error);
    console.error('Error stack:', error.stack);
  }
  } catch (error) {
    console.error('Error in renderSingleTokenChart:', error);
  }
}

// Render chart for multiple tokens
function renderMultiTokenChart(ctx, data) {
  console.log('Rendering multi-token chart');
  
  // Extract unique token tickers
  const tokenTickers = data.length > 0 && data[0].tokens 
    ? Object.keys(data[0].tokens).filter(ticker => ticker !== 'JitoSOL')
    : [];
  
  // Color palette for different tokens
  const colors = [
    '#ffc0cb', // pink
    '#ffb6c1', // light pink
    '#f59e0b', // amber
    '#ef4444', // red
    '#8b5cf6', // violet
    '#06b6d4', // cyan
    '#ec4899', // pink
    '#14b8a6'  // teal
  ];
  
  const datasets = [];
  
  // Add JitoSOL/FeelsSOL as line chart
  const jitoData = data.map((point) => {
    const block = Number(point.block) || 0;  // Use block number
    const price = point.price || 1.0; // Default JitoSOL/FeelsSOL price
    
    return {
      x: block,  // Use block number for x-axis
      y: price
    };
  });
  
  datasets.push({
    label: 'JitoSOL/FeelsSOL',
    type: 'line',
    data: jitoData,
    borderColor: '#000000',
    backgroundColor: 'rgba(0, 0, 0, 0.1)',
    borderWidth: 2,
    pointRadius: 1,
    tension: 0.1,
    fill: false
  });
  
  // Add each feels token as a line
  tokenTickers.forEach((ticker, index) => {
    const tokenData = data.map(point => ({
      x: Number(point.block) || 0,  // Use block number
      y: point.tokens && point.tokens[ticker] ? point.tokens[ticker].price : 0
    }));
    
    datasets.push({
      label: `${ticker}/FeelsSOL`,
      type: 'line',
      data: tokenData,
      borderColor: colors[index % colors.length],
      backgroundColor: 'transparent',
      borderWidth: 2,
      pointRadius: 0,
      tension: 0.1
    });
    
    // Add NFV floor for this token
    const nfvData = data.map(point => ({
      x: Number(point.block) || 0,  // Use block number
      y: point.tokens && point.tokens[ticker] ? point.tokens[ticker].nfvFloor : 0
    }));
    
    datasets.push({
      label: `${ticker} NFV Floor`,
      type: 'line',
      data: nfvData,
      borderColor: colors[index % colors.length],
      backgroundColor: colors[index % colors.length] + '20', // 20% opacity
      borderWidth: 1,
      borderDash: [5, 5],
      pointRadius: 0,
      fill: true,
      tension: 0.1
    });
  });
  
  // Create the chart
  chartInstance = new Chart(ctx, {
    type: 'line',
    data: {
      datasets: datasets
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      animation: false,
      interaction: {
        mode: 'index',
        intersect: false
      },
      plugins: {
        legend: {
          display: true,
          position: 'top',
          labels: {
            color: '#000000',
            font: {
              family: 'monospace',
              size: 11
            },
            boxWidth: 15,
            padding: 10,
            usePointStyle: true
          }
        },
        tooltip: {
          mode: 'index',
          intersect: false,
          backgroundColor: 'rgba(0, 0, 0, 0.9)',
          titleColor: '#e5e7eb',
          bodyColor: '#e5e7eb',
          borderColor: '#374151',
          borderWidth: 1,
          padding: 10,
          displayColors: true,
          callbacks: {
            label: function(context) {
              if (!context.parsed || context.parsed.y == null) {
                return 'No data';
              }
              return `${context.dataset.label}: ${context.parsed.y.toFixed(4)}`;
            }
          }
        }
      },
      scales: {
        x: {
          type: 'linear',
          title: {
            display: true,
            text: 'Block Number',
            color: '#000000',
            font: {
              family: 'monospace'
            }
          },
          min: data.length > 0 ? Math.min(...data.map(d => d.block || 0)) : 0,
          max: data.length > 0 ? Math.max(...data.map(d => d.block || 0)) : 100,
          grid: {
            color: '#1f2937',
            drawBorder: false
          },
          ticks: {
            color: '#000000',
            font: {
              family: 'monospace',
              size: 11
            },
            autoSkip: true,
            maxTicksLimit: 10
          }
        },
        y: {
          position: 'left',
          grid: {
            color: '#1f2937',
            drawBorder: false
          },
          ticks: {
            color: '#000000',
            font: {
              family: 'monospace',
              size: 11
            },
            callback: function(value) {
              return (value != null && typeof value === 'number') ? value.toFixed(3) : '0.000';
            }
          },
          title: {
            display: true,
            text: 'Price vs FeelsSOL',
            color: '#000000'
          }
        }
      }
    }
  });
  
  console.log('Multi-token chart created successfully');
  console.log('Chart instance available:', !!chartInstance);
  console.log('Chart datasets:', chartInstance?.data?.datasets?.length);
  
  // Make chart instance globally available for debugging
  window.currentChartInstance = chartInstance;
}

// Set innerHTML by element ID
export const setElementInnerHTML = function(elementId) {
  return function(html) {
    return function() {
      const element = document.getElementById(elementId);
      if (element) {
        element.innerHTML = html;
      }
    };
  };
};

// Set attribute by element ID
export const setElementAttribute = function(elementId) {
  return function(attrName) {
    return function(attrValue) {
      return function() {
        const element = document.getElementById(elementId);
        if (element) {
          element.setAttribute(attrName, attrValue);
        }
      };
    };
  };
};

// Remove attribute by element ID
export const removeElementAttribute = function(elementId) {
  return function(attrName) {
    return function() {
      const element = document.getElementById(elementId);
      if (element) {
        element.removeAttribute(attrName);
      }
    };
  };
};

// Register getChartData action when DOM is ready
if (typeof window !== 'undefined') {
  window.addEventListener('DOMContentLoaded', function() {
    setTimeout(function() {
      if (window.remoteControl && window.remoteControl.registerAction) {
        window.remoteControl.registerAction('getChartData', function() {
          console.log('Remote action: getChartData triggered');
          const chart = window.currentChartInstance || chartInstance;
          
          if (chart && chart.data && chart.data.datasets) {
            const datasets = chart.data.datasets.map(ds => ({
              label: ds.label,
              data: ds.data ? ds.data.slice(0, 20) : [], // Limit to first 20 points
              type: ds.type || chart.config.type
            }));
            
            // Log analysis for each dataset
            datasets.forEach(ds => {
              if (ds.data && ds.data.length > 0) {
                const values = ds.data.map(d => {
                  // Handle different data formats
                  if (typeof d === 'number') return d;
                  if (d.y !== undefined) return d.y;
                  if (d.close !== undefined) return d.close;
                  if (d.c !== undefined) return d.c;
                  return 0;
                }).filter(v => !isNaN(v));
                
                if (values.length > 0) {
                  const min = Math.min(...values);
                  const max = Math.max(...values);
                  const avg = values.reduce((a, b) => a + b, 0) / values.length;
                  console.log(`${ds.label}: ${ds.data.length} points, range: ${min.toFixed(4)} - ${max.toFixed(4)}, avg: ${avg.toFixed(4)}`);
                }
              }
            });
            
            return { success: true, datasets: datasets };
          } else {
            console.log('Chart data element not found');
            return { success: false, error: 'No chart data available' };
          }
        });
        console.log('Registered getChartData action');
      }
    }, 1000); // Give time for remote control to initialize
  });
}