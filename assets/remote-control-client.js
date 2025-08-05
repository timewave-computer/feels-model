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
    
    // Store reference to UI actions
    window.remoteControl = {
        actions: {},
        registerAction: function(name, handler) {
            this.actions[name] = handler;
            console.log(`Remote action registered: ${name}`);
        },
        executeAction: function(name, params) {
            if (this.actions[name]) {
                console.log(`Executing remote action: ${name}`, params);
                return this.actions[name](params);
            } else {
                console.error(`Unknown remote action: ${name}`);
                return Promise.reject(`Unknown action: ${name}`);
            }
        }
    };
    
    function connect() {
        if (isConnecting || (ws && ws.readyState === WebSocket.OPEN)) {
            return;
        }
        
        isConnecting = true;
        console.log('Connecting to remote control server...');
        
        try {
            ws = new WebSocket(WS_URL);
            
            ws.onopen = function() {
                originalLog.call(console, 'Connected to remote control server'); // Use original to avoid recursion
                isConnecting = false;
                
                // Clear reconnect interval
                if (reconnectInterval) {
                    clearInterval(reconnectInterval);
                    reconnectInterval = null;
                }
                
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
                    } else {
                        // Legacy format - assume it's an action command
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
                    }
                } catch (e) {
                    console.error('Error processing message:', e);
                }
            };
            
            ws.onerror = function(error) {
                console.error('WebSocket error:', error);
                isConnecting = false;
            };
            
            ws.onclose = function() {
                console.log('Disconnected from remote control server');
                isConnecting = false;
                ws = null;
                
                // Start reconnection attempts
                if (!reconnectInterval) {
                    console.log('Will attempt to reconnect in 5 seconds...');
                    reconnectInterval = setInterval(connect, 5000);
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
    
    // Log that remote control is active
    console.log('Remote control client initialized - waiting for UI to register actions');
})();