// Console log mirroring to backend server
// Intercepts browser console.log and sends to local dev server

(function() {
    'use strict';

    // Only enable in development (localhost)
    if (window.location.hostname !== 'localhost' && window.location.hostname !== '127.0.0.1') {
        return;
    }

    // Store original console methods
    const originalLog = console.log;
    const originalWarn = console.warn;
    const originalError = console.error;
    const originalInfo = console.info;

    // Log server configuration
    const LOG_SERVER_PORT = 3001;
    const LOG_SERVER_URL = `http://localhost:${LOG_SERVER_PORT}/__log`;
    
    // Rate limiting
    let logQueue = [];
    let isProcessing = false;
    const MAX_LOGS_PER_BATCH = 10;
    const BATCH_INTERVAL = 100; // ms

    // Process log queue in batches
    function processLogQueue() {
        if (isProcessing || logQueue.length === 0) return;
        
        isProcessing = true;
        const batch = logQueue.splice(0, MAX_LOGS_PER_BATCH);
        
        // Send batch to server
        try {
            fetch(LOG_SERVER_URL, {
                method: 'POST',
                headers: { 
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    batch: batch,
                    url: window.location.href,
                    userAgent: navigator.userAgent.substring(0, 100)
                }),
            }).catch(() => {
                // Silently fail
            }).finally(() => {
                isProcessing = false;
                if (logQueue.length > 0) {
                    setTimeout(processLogQueue, BATCH_INTERVAL);
                }
            });
        } catch (e) {
            isProcessing = false;
        }
    }

    // Helper to queue logs
    function queueLog(level, args) {
        try {
            const logData = {
                level: level,
                timestamp: new Date().toISOString(),
                logs: args.map(arg => {
                    if (typeof arg === 'object') {
                        try {
                            return JSON.stringify(arg, null, 2);
                        } catch (e) {
                            return '[Object (circular)]';
                        }
                    }
                    return String(arg);
                })
            };
            
            logQueue.push(logData);
            
            // Start processing if not already
            if (!isProcessing) {
                setTimeout(processLogQueue, 10);
            }
        } catch (e) {
            // Silently handle any errors
        }
    }

    // Override console.log
    console.log = function(...args) {
        queueLog('LOG', args);
        originalLog.apply(console, args);
    };

    // Override console.warn
    console.warn = function(...args) {
        queueLog('WARN', args);
        originalWarn.apply(console, args);
    };

    // Override console.error
    console.error = function(...args) {
        queueLog('ERROR', args);
        originalError.apply(console, args);
    };

    // Override console.info
    console.info = function(...args) {
        queueLog('INFO', args);
        originalInfo.apply(console, args);
    };

    // Also capture uncaught errors
    window.addEventListener('error', function(event) {
        queueLog('UNCAUGHT_ERROR', [
            `${event.message} at ${event.filename}:${event.lineno}:${event.colno}`,
            event.error?.stack || 'No stack trace'
        ]);
    });

    // Capture unhandled promise rejections
    window.addEventListener('unhandledrejection', function(event) {
        queueLog('UNHANDLED_REJECTION', [
            'Unhandled Promise Rejection:',
            event.reason
        ]);
    });

    // Log that mirroring is active
    console.log('Console log mirroring is active - logs will be sent to backend');

})();