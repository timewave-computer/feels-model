// Remote control client for browser
// Connects to WebSocket server and executes commands

(function() {
    'use strict';
    
    // Only enable in development
    if (window.location.hostname !== 'localhost' && window.location.hostname !== '127.0.0.1') {
        return;
    }
    
    // Store original console methods immediately
    const originalLog = console.log;
    const originalError = console.error;
    const originalWarn = console.warn;
    
    // Create a buffer for messages sent before connection
    const messageBuffer = [];
    const MAX_BUFFER_SIZE = 100;
    
    const WS_URL = 'ws://localhost:3002';
    let ws = null;
    let reconnectInterval = null;
    let isConnecting = false;
    let reconnectAttempts = 0;
    let heartbeatInterval = null;
    let lastPongTime = Date.now();
    const MAX_RECONNECT_ATTEMPTS = 10;
    const HEARTBEAT_INTERVAL = 30000; // 30 seconds
    const PONG_TIMEOUT = 5000; // 5 seconds to respond to ping
    
    // Console forwarding function
    function forwardConsoleMessage(level, args) {
        // Always call original console method first
        const original = level === 'log' ? originalLog : 
                        level === 'error' ? originalError : originalWarn;
        original.apply(console, args);
        
        // Create message object
        const message = {
            type: 'console',
            level: level,
            message: Array.from(args).map(arg => {
                if (typeof arg === 'object') {
                    try {
                        return JSON.stringify(arg, null, 2);
                    } catch (e) {
                        return String(arg);
                    }
                }
                return String(arg);
            }).join(' '),
            timestamp: Date.now()
        };
        
        // Send immediately if connected, otherwise buffer
        if (ws && ws.readyState === WebSocket.OPEN) {
            try {
                ws.send(JSON.stringify(message));
            } catch (e) {
                // Ignore send errors
            }
        } else {
            // Buffer messages when not connected
            messageBuffer.push(message);
            if (messageBuffer.length > MAX_BUFFER_SIZE) {
                messageBuffer.shift(); // Remove oldest
            }
        }
    }
    
    // Function to flush buffered messages
    function flushMessageBuffer() {
        if (ws && ws.readyState === WebSocket.OPEN && messageBuffer.length > 0) {
            originalLog.call(console, `Flushing ${messageBuffer.length} buffered messages`);
            messageBuffer.forEach(msg => {
                try {
                    ws.send(JSON.stringify(msg));
                } catch (e) {
                    // Ignore send errors
                }
            });
            messageBuffer.length = 0; // Clear buffer
        }
    }
    
    // Override console methods immediately to capture all logs
    console.log = function() {
        forwardConsoleMessage('log', arguments);
    };
    
    console.error = function() {
        forwardConsoleMessage('error', arguments);
    };
    
    console.warn = function() {
        forwardConsoleMessage('warn', arguments);
    };
    
    // Test that console override is working
    console.log('Console override active - this should be forwarded');
    
    // Heartbeat mechanism
    function startHeartbeat() {
        stopHeartbeat(); // Clear any existing heartbeat
        
        heartbeatInterval = setInterval(() => {
            if (ws && ws.readyState === WebSocket.OPEN) {
                // Check if we've received a pong recently
                if (Date.now() - lastPongTime > HEARTBEAT_INTERVAL + PONG_TIMEOUT) {
                    console.warn('WebSocket appears unresponsive, closing connection');
                    ws.close();
                    return;
                }
                
                // Send ping
                try {
                    ws.send(JSON.stringify({ type: 'ping', timestamp: Date.now() }));
                } catch (e) {
                    console.error('Failed to send ping:', e);
                }
            }
        }, HEARTBEAT_INTERVAL);
    }
    
    function stopHeartbeat() {
        if (heartbeatInterval) {
            clearInterval(heartbeatInterval);
            heartbeatInterval = null;
        }
    }
    
    // Store reference to UI actions
    window.remoteControl = {
        actions: {},
        connectionState: 'disconnected',
        registerAction: function(name, handler) {
            this.actions[name] = handler;
            console.log(`Remote action registered: ${name}`);
        },
        executeAction: function(name, params) {
            if (this.actions[name]) {
                console.log(`Executing remote action: ${name}`, params);
                try {
                    return this.actions[name](params);
                } catch (error) {
                    console.error(`Error executing action ${name}:`, error);
                    return Promise.reject(error);
                }
            } else {
                console.error(`Unknown remote action: ${name}`);
                return Promise.reject(`Unknown action: ${name}`);
            }
        },
        getConnectionState: function() {
            return this.connectionState;
        },
        isConnected: function() {
            return ws && ws.readyState === WebSocket.OPEN;
        },
        reconnect: function() {
            console.log('Manual reconnect requested');
            reconnectAttempts = 0; // Reset attempts for manual reconnect
            if (ws) {
                ws.close();
            }
            connect();
        },
        disconnect: function() {
            console.log('Manual disconnect requested');
            reconnectAttempts = MAX_RECONNECT_ATTEMPTS; // Prevent auto-reconnect
            if (ws) {
                ws.close();
            }
        }
    };
    
    function connect() {
        if (isConnecting || (ws && ws.readyState === WebSocket.OPEN)) {
            return;
        }
        
        // Check max reconnection attempts
        if (reconnectAttempts >= MAX_RECONNECT_ATTEMPTS) {
            console.error(`Failed to connect after ${MAX_RECONNECT_ATTEMPTS} attempts. Giving up.`);
            window.remoteControl.connectionState = 'failed';
            return;
        }
        
        isConnecting = true;
        window.remoteControl.connectionState = 'connecting';
        console.log(`Connecting to remote control server... (attempt ${reconnectAttempts + 1}/${MAX_RECONNECT_ATTEMPTS})`);
        
        try {
            ws = new WebSocket(WS_URL);
            
            ws.onopen = function() {
                originalLog.call(console, 'Connected to remote control server'); // Use original to avoid recursion
                isConnecting = false;
                reconnectAttempts = 0; // Reset reconnection attempts on successful connection
                window.remoteControl.connectionState = 'connected';
                lastPongTime = Date.now(); // Reset pong timer
                
                // Clear reconnect interval
                if (reconnectInterval) {
                    clearInterval(reconnectInterval);
                    reconnectInterval = null;
                }
                
                // Start heartbeat
                startHeartbeat();
                
                // Send ready message
                ws.send(JSON.stringify({
                    type: 'ready',
                    timestamp: Date.now()
                }));
                
                // Flush any buffered console messages
                setTimeout(flushMessageBuffer, 100);
            };
            
            ws.onmessage = async function(event) {
                try {
                    const message = JSON.parse(event.data);
                    
                    // Handle pong messages for heartbeat
                    if (message.type === 'pong') {
                        lastPongTime = Date.now();
                        return;
                    }
                    
                    console.log('Received message:', message);
                    
                    if (message.type === 'command') {
                        // Handle UI commands - use action from message, not command
                        console.log('Executing command:', message.action, 'with params:', message.params);
                        try {
                            if (!window.remoteControl) {
                                throw new Error('remoteControl not initialized');
                            }
                            if (!window.remoteControl.actions[message.action]) {
                                throw new Error(`Action not registered: ${message.action}`);
                            }
                            
                            const result = await window.remoteControl.executeAction(message.action, message.params || {});
                            console.log('Command result:', result);
                            
                            // Send success response
                            ws.send(JSON.stringify({
                                type: 'response',
                                commandId: message.id,
                                status: 'success',
                                result: result
                            }));
                        } catch (error) {
                            console.error('Command error:', error);
                            // Send error response
                            ws.send(JSON.stringify({
                                type: 'response',
                                commandId: message.id,
                                status: 'error',
                                error: error.toString()
                            }));
                        }
                    } else if (message.type === 'execute') {
                        // Handle code execution without eval for CSP compliance
                        try {
                            // For now, just log and respond
                            console.log('Execute request (CSP prevents eval):', message.code);
                            
                            // Send response indicating CSP restriction
                            ws.send(JSON.stringify({
                                type: 'response',
                                commandId: message.id,
                                status: 'error',
                                error: 'CSP prevents eval. Use registered actions instead.'
                            }));
                        } catch (error) {
                            // Send error response
                            ws.send(JSON.stringify({
                                type: 'response',
                                commandId: message.id,
                                status: 'error',
                                error: error.toString()
                            }));
                        }
                    } else if (message.type === 'console') {
                        // This is a console message forwarded from another client - ignore
                        // We don't need to process console messages as commands
                        return;
                    } else {
                        // Legacy format - assume it's an action command only if it has an action field
                        if (message.action) {
                            try {
                                const result = await window.remoteControl.executeAction(message.action, message.params);
                                
                                // Send success response
                                ws.send(JSON.stringify({
                                    type: 'response',
                                    commandId: message.id,
                                    status: 'success',
                                    result: result
                                }));
                            } catch (error) {
                                // Send error response
                                ws.send(JSON.stringify({
                                    type: 'response',
                                    commandId: message.id,
                                    status: 'error',
                                    error: error.toString()
                                }));
                            }
                        } else {
                            // Unknown message type without action - just log and ignore
                            console.log('Received unknown message type:', message);
                        }
                    }
                } catch (e) {
                    console.error('Error processing message:', e);
                }
            };
            
            ws.onerror = function(error) {
                console.error('WebSocket error:', error);
                isConnecting = false;
                window.remoteControl.connectionState = 'error';
            };
            
            ws.onclose = function(event) {
                console.log('Disconnected from remote control server', event.code, event.reason);
                isConnecting = false;
                window.remoteControl.connectionState = 'disconnected';
                ws = null;
                
                // Stop heartbeat
                stopHeartbeat();
                
                // Start reconnection attempts with exponential backoff
                if (!reconnectInterval && reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
                    reconnectAttempts++;
                    // Exponential backoff: 1s, 2s, 4s, 8s, 16s, 32s, then cap at 60s
                    const backoffDelay = Math.min(Math.pow(2, reconnectAttempts - 1) * 1000, 60000);
                    console.log(`Will attempt to reconnect in ${backoffDelay / 1000} seconds...`);
                    reconnectInterval = setTimeout(() => {
                        reconnectInterval = null;
                        connect();
                    }, backoffDelay);
                }
            };
        } catch (e) {
            console.error('Failed to create WebSocket:', e);
            isConnecting = false;
        }
    }
    
    // Initial connection
    connect();
    
    // Register built-in actions
    window.remoteControl.registerAction('ping', () => {
        console.log('Ping action called');
        return { message: 'pong', timestamp: Date.now() };
    });
    
    window.remoteControl.registerAction('test', () => {
        console.log('Test action called');
        return { success: true, message: 'Test successful' };
    });
    
    window.remoteControl.registerAction('reload', () => {
        window.location.reload();
        return { message: 'Reloading page...' };
    });
    
    window.remoteControl.registerAction('getStats', () => {
        return {
            url: window.location.href,
            readyState: document.readyState,
            hasUI: !!document.getElementById('app'),
            timestamp: Date.now()
        };
    });
    
    window.remoteControl.registerAction('getConnectionStatus', () => {
        return {
            state: window.remoteControl.connectionState,
            isConnected: window.remoteControl.isConnected(),
            reconnectAttempts: reconnectAttempts,
            maxReconnectAttempts: MAX_RECONNECT_ATTEMPTS,
            lastPongTime: lastPongTime,
            timeSinceLastPong: Date.now() - lastPongTime,
            wsReadyState: ws ? ws.readyState : null,
            wsReadyStateString: ws ? ['CONNECTING', 'OPEN', 'CLOSING', 'CLOSED'][ws.readyState] : 'NO_SOCKET'
        };
    });
    
    window.remoteControl.registerAction('eval', (params) => {
        // CSP-compliant version - no eval allowed
        return { 
            error: 'eval() not allowed due to CSP restrictions. Use specific actions instead.',
            suggestion: 'Register specific actions for your use case'
        };
    });
    
    window.remoteControl.registerAction('debugChart', () => {
        return {
            hasCurrentChartInstance: !!window.currentChartInstance,
            currentChartDatasets: window.currentChartInstance?.data?.datasets?.length || 0,
            hasChartInstance: typeof chartInstance !== 'undefined',
            chartInstanceDatasets: typeof chartInstance !== 'undefined' && chartInstance?.data?.datasets?.length || 0
        };
    });
    
    window.remoteControl.registerAction('getChartData', () => {
        const chart = window.currentChartInstance;
        if (!chart || !chart.data || !chart.data.datasets) {
            return { success: false, error: 'No chart data available' };
        }
        
        const datasets = chart.data.datasets.map(ds => ({
            label: ds.label,
            dataLength: ds.data ? ds.data.length : 0,
            sampleData: ds.data ? ds.data.slice(0, 5) : []
        }));
        
        return { 
            success: true, 
            datasets: datasets,
            totalDatasets: datasets.length
        };
    });
    
    window.remoteControl.registerAction('checkChart', () => {
        const result = {
            chartDefined: typeof Chart !== 'undefined',
            version: typeof Chart !== 'undefined' ? Chart.version : 'not loaded',
            hasRegistry: typeof Chart !== 'undefined' && Chart.registry ? true : false
        };
        
        if (result.hasRegistry) {
            result.registryKeys = Object.keys(Chart.registry);
            
            if (Chart.registry.controllers) {
                const controllers = Chart.registry.controllers;
                result.controllersType = typeof controllers;
                result.controllersKeys = Object.keys(controllers);
                
                // Check both controllers and controllers.items for the candlestick controller
                const items = controllers.items || {};
                result.itemsKeys = Object.keys(items);
                result.hasCandlestick = 'candlestick' in items || 'candlestick' in controllers;
                result.hasLine = 'line' in items || 'line' in controllers;
                result.hasBar = 'bar' in items || 'bar' in controllers;
                
                // Log detailed info
                console.log('Controllers object:', controllers);
                console.log('Items object:', items);
            }
        }
        
        return result;
    });
    
    window.remoteControl.registerAction('debugChartComplete', () => {
        console.log('=== COMPLETE CHART DEBUG ===');
        
        // Check functions
        const funcs = {
            checkAndInitializeChart: typeof checkAndInitializeChart !== 'undefined',
            initializePriceChart: typeof initializePriceChart !== 'undefined',
            windowCheck: typeof window.checkAndInitializeChart !== 'undefined',
            windowInit: typeof window.initializePriceChart !== 'undefined'
        };
        console.log('Functions available:', funcs);
        
        // Check DOM
        const canvas = document.getElementById('price-chart');
        const dataEl = document.getElementById('chart-data-hidden');
        const dom = {
            canvasFound: !!canvas,
            dataFound: !!dataEl,
            canvasSize: canvas ? canvas.width + 'x' + canvas.height : 'none',
            canvasOffset: canvas ? canvas.offsetWidth + 'x' + canvas.offsetHeight : 'none',
            dataLength: dataEl ? dataEl.textContent.length : 0,
            dataPreview: dataEl ? dataEl.textContent.substring(0, 100) : 'none'
        };
        console.log('DOM elements:', dom);
        
        // Try to call chart init
        if (window.checkAndInitializeChart) {
            console.log('Calling window.checkAndInitializeChart...');
            try {
                window.checkAndInitializeChart()();
            } catch (e) {
                console.error('Error:', e);
            }
        }
        
        return { functions: funcs, dom: dom };
    });
    
    // Log that remote control is active
    console.log('Remote control client initialized - waiting for UI to register actions');
})();