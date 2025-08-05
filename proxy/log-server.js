#!/usr/bin/env node

// Simple Express server to receive browser console logs
// Runs on port 3001 and saves logs to browser.log

const express = require('express');
const fs = require('fs');
const path = require('path');

const app = express();
const PORT = 3001;
const LOG_DIR = path.join(__dirname, '..', 'logs');
const LOG_FILE = path.join(LOG_DIR, 'browser-console.log');

// Middleware
app.use(express.json({ limit: '10mb' }));

// Enable CORS for localhost development
app.use((req, res, next) => {
    res.header('Access-Control-Allow-Origin', '*');
    res.header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept');
    
    if (req.method === 'OPTIONS') {
        res.sendStatus(200);
    } else {
        next();
    }
});

// Color codes for terminal output
const colors = {
    LOG: '\x1b[37m',     // White
    INFO: '\x1b[36m',    // Cyan
    WARN: '\x1b[33m',    // Yellow
    ERROR: '\x1b[31m',   // Red
    UNCAUGHT_ERROR: '\x1b[41m\x1b[37m', // Red background, white text
    UNHANDLED_REJECTION: '\x1b[45m\x1b[37m', // Magenta background, white text
    reset: '\x1b[0m'     // Reset
};

// Format timestamp for display
function formatTimestamp(isoString) {
    const date = new Date(isoString);
    return date.toLocaleTimeString('en-US', { 
        hour12: false, 
        hour: '2-digit', 
        minute: '2-digit', 
        second: '2-digit',
        fractionalSecondDigits: 3
    });
}

// Process a single log entry
function processLog(level, timestamp, logs, url, userAgent) {
    // Format for terminal output
    const time = formatTimestamp(timestamp);
    const color = colors[level] || colors.LOG;
    const levelPadded = level.padEnd(8);
    const logContent = logs.join(' ');
    
    // Terminal output with colors
    console.log(`${color}[${time}] ${levelPadded}${colors.reset} ${logContent}`);
    
    // If it's an error or warning, also show the URL
    if (level === 'ERROR' || level === 'WARN' || level === 'UNCAUGHT_ERROR' || level === 'UNHANDLED_REJECTION') {
        console.log(`${colors.reset}${' '.repeat(19)}↳ ${url}`);
    }

    // Format for file output (no colors, more detail)
    let fileLogLine = `[${timestamp}] ${level} ${logContent}\n`;
    if (level === 'ERROR' || level === 'WARN' || level === 'UNCAUGHT_ERROR' || level === 'UNHANDLED_REJECTION') {
        fileLogLine += `    URL: ${url}\n`;
        if (userAgent) {
            fileLogLine += `    UA: ${userAgent}\n`;
        }
    }

    // Append to log file
    fs.appendFileSync(LOG_FILE, fileLogLine);
}

// POST endpoint to receive logs
app.post('/__log', (req, res) => {
    try {
        const { batch, level, timestamp, logs, url, userAgent } = req.body;
        
        // Handle batched logs
        if (batch && Array.isArray(batch)) {
            batch.forEach(logItem => {
                processLog(logItem.level, logItem.timestamp, logItem.logs, url, userAgent);
            });
            return res.json({ status: 'batch logged', count: batch.length });
        }
        
        // Handle single log (backward compatibility)
        if (!logs || !Array.isArray(logs)) {
            return res.status(400).json({ error: 'Invalid log data' });
        }

        processLog(level, timestamp, logs, url, userAgent);
        res.json({ status: 'logged' });
    } catch (error) {
        console.error('Error processing log:', error);
        res.status(500).json({ error: 'Failed to process log' });
    }
});

// Health check endpoint
app.get('/__health', (req, res) => {
    res.json({ 
        status: 'ok', 
        port: PORT,
        logFile: LOG_FILE,
        uptime: process.uptime()
    });
});

// Start server
app.listen(PORT, () => {
    console.log(`\nBrowser Log Mirror Server`);
    console.log(`Running on http://localhost:${PORT}`);
    console.log(`Logs saved to: ${LOG_FILE}`);
    console.log(`To tail logs: tail -f logs/browser-console.log`);
    console.log(`To stop: Ctrl+C\n`);
    
    // Create logs directory and log file if they don't exist
    if (!fs.existsSync(LOG_DIR)) {
        fs.mkdirSync(LOG_DIR, { recursive: true });
    }
    if (!fs.existsSync(LOG_FILE)) {
        fs.writeFileSync(LOG_FILE, `# Browser Console Logs - Started ${new Date().toISOString()}\n`);
    }
});

// Graceful shutdown
process.on('SIGINT', () => {
    console.log('\nShutting down log server...');
    process.exit(0);
});

process.on('SIGTERM', () => {
    console.log('\nShutting down log server...');
    process.exit(0);
});