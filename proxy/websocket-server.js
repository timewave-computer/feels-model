#!/usr/bin/env node

// WebSocket server for remote control of browser app
// Allows CLI to send commands to connected browser instances

const WebSocket = require('ws');
const http = require('http');
const express = require('express');
const fs = require('fs');
const path = require('path');

const app = express();
app.use(express.json());

const server = http.createServer(app);
const wss = new WebSocket.Server({ server });

const PORT = 3002;

// Setup logging to logs/ directory
const logsDir = path.join(__dirname, '..', 'logs');
const logFile = path.join(logsDir, 'feels-websocket-server.log');

// Ensure logs directory exists
if (!fs.existsSync(logsDir)) {
    fs.mkdirSync(logsDir, { recursive: true });
}

// Custom logging function that writes to both console and file
function log(...args) {
    const timestamp = new Date().toISOString();
    const message = args.join(' ');
    const logLine = `[${timestamp}] ${message}\n`;
    
    // Write to console
    console.log(message);
    
    // Append to log file
    try {
        fs.appendFileSync(logFile, logLine);
    } catch (error) {
        console.error('Failed to write to log file:', error);
    }
}
let connectedClients = new Set();
let commandQueue = [];
let commandId = 0;

// Store WebSocket clients that are listening for console output
let consoleListeners = new Set();

// Track command senders to route responses back
let commandSenders = new Map(); // commandId -> sender WebSocket

// Track connected clients
wss.on('connection', (ws) => {
    log('Browser client connected');
    connectedClients.add(ws);
    
    // Send any queued commands
    if (commandQueue.length > 0) {
        commandQueue.forEach(cmd => {
            ws.send(JSON.stringify(cmd));
        });
        commandQueue = [];
    }
    
    ws.on('message', (message) => {
        try {
            const data = JSON.parse(message);
            
            // Handle different message types
            if (data.type === 'ping') {
                // Respond to ping with pong
                ws.send(JSON.stringify({ type: 'pong', timestamp: Date.now() }));
            } else if (data.type === 'command') {
                // Relay command from non-browser client to browser clients
                log(`Relaying command ${data.id} to browser clients:`, data.action);
                
                // Store the sender so we can route the response back
                commandSenders.set(data.id, ws);
                
                let commandSent = false;
                connectedClients.forEach(client => {
                    // Send to all clients except the sender (assuming browser clients don't send commands)
                    if (client !== ws && client.readyState === WebSocket.OPEN) {
                        client.send(JSON.stringify(data));
                        commandSent = true;
                    }
                });
                if (!commandSent) {
                    // No browser clients to send to
                    ws.send(JSON.stringify({
                        type: 'response',
                        commandId: data.id,
                        status: 'error',
                        error: 'No browser clients connected'
                    }));
                    commandSenders.delete(data.id);
                }
            } else if (data.type === 'console') {
                // Format console messages
                const timestamp = new Date(data.timestamp).toLocaleTimeString();
                const level = data.level.toUpperCase().padEnd(5);
                log(`[${timestamp}] [${level}] ${data.message}`);
                
                // Broadcast console messages to all other connected clients
                connectedClients.forEach(client => {
                    if (client !== ws && client.readyState === WebSocket.OPEN) {
                        client.send(JSON.stringify(data));
                    }
                });
            } else if (data.type === 'response') {
                log(`Command ${data.commandId} completed:`, data.status);
                if (data.result) {
                    log('Result:', JSON.stringify(data.result, null, 2));
                }
                if (data.error) {
                    log('Error:', data.error);
                }
                
                // Route response back to command sender
                const sender = commandSenders.get(data.commandId);
                if (sender && sender.readyState === WebSocket.OPEN) {
                    sender.send(JSON.stringify(data));
                    commandSenders.delete(data.commandId);
                }
            } else {
                log('Received from browser:', data);
            }
        } catch (e) {
            log('Error parsing message:', e);
        }
    });
    
    ws.on('close', () => {
        log('Browser client disconnected');
        connectedClients.delete(ws);
    });
    
    ws.on('error', (error) => {
        log('WebSocket error:', error);
        connectedClients.delete(ws);
    });
});

// HTTP endpoint to send commands
app.post('/command', (req, res) => {
    const { action, params } = req.body;
    
    if (!action) {
        return res.status(400).json({ error: 'Action is required' });
    }
    
    const command = {
        type: 'command',
        id: ++commandId,
        action,
        params: params || {},
        timestamp: Date.now()
    };
    
    log(`Sending command to ${connectedClients.size} clients:`, command);
    
    if (connectedClients.size === 0) {
        // Queue command for when a client connects
        commandQueue.push(command);
        return res.json({ 
            status: 'queued', 
            message: 'No clients connected. Command queued for next connection.',
            commandId: command.id 
        });
    }
    
    // Send to all connected clients
    let sent = 0;
    connectedClients.forEach(client => {
        if (client.readyState === WebSocket.OPEN) {
            client.send(JSON.stringify(command));
            sent++;
        }
    });
    
    res.json({ 
        status: 'sent', 
        clientCount: sent,
        commandId: command.id 
    });
});

// Health check endpoint
app.get('/status', (req, res) => {
    res.json({
        connected: connectedClients.size,
        queued: commandQueue.length,
        uptime: process.uptime()
    });
});

// Start server
server.listen(PORT, () => {
    log(`\nRemote Control Server`);
    log(`WebSocket: ws://localhost:${PORT}`);
    log(`HTTP API: http://localhost:${PORT}`);
    log(`Log file: ${logFile}`);
    log(`\nAvailable endpoints:`);
    log(`  POST /command - Send command to browser`);
    log(`  GET /status - Check server status`);
    log(`\nExample command:`);
    log(`  curl -X POST http://localhost:${PORT}/command \\`);
    log(`    -H "Content-Type: application/json" \\`);
    log(`    -d '{"action": "runSimulation", "params": {}}'`);
    log(`\nWaiting for connections...\n`);
});

// Graceful shutdown
process.on('SIGINT', () => {
    log('\nShutting down remote control server...');
    wss.close(() => {
        server.close(() => {
            process.exit(0);
        });
    });
});