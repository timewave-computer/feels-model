#!/usr/bin/env node

// WebSocket server for remote control of browser app
// Allows CLI to send commands to connected browser instances

const WebSocket = require('ws');
const http = require('http');
const express = require('express');

const app = express();
app.use(express.json());

const server = http.createServer(app);
const wss = new WebSocket.Server({ server });

const PORT = 3002;
let connectedClients = new Set();
let commandQueue = [];
let commandId = 0;

// Store WebSocket clients that are listening for console output
let consoleListeners = new Set();

// Track connected clients
wss.on('connection', (ws) => {
    console.log('Browser client connected');
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
            if (data.type === 'console') {
                // Format console messages
                const timestamp = new Date(data.timestamp).toLocaleTimeString();
                const level = data.level.toUpperCase().padEnd(5);
                console.log(`[${timestamp}] [${level}] ${data.message}`);
                
                // Broadcast console messages to all other connected clients
                connectedClients.forEach(client => {
                    if (client !== ws && client.readyState === WebSocket.OPEN) {
                        client.send(JSON.stringify(data));
                    }
                });
            } else if (data.type === 'response') {
                console.log(`Command ${data.commandId} completed:`, data.status);
                if (data.result) {
                    console.log('Result:', JSON.stringify(data.result, null, 2));
                }
                if (data.error) {
                    console.error('Error:', data.error);
                }
            } else {
                console.log('Received from browser:', data);
            }
        } catch (e) {
            console.error('Error parsing message:', e);
        }
    });
    
    ws.on('close', () => {
        console.log('Browser client disconnected');
        connectedClients.delete(ws);
    });
    
    ws.on('error', (error) => {
        console.error('WebSocket error:', error);
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
    
    console.log(`Sending command to ${connectedClients.size} clients:`, command);
    
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
    console.log(`\nRemote Control Server`);
    console.log(`WebSocket: ws://localhost:${PORT}`);
    console.log(`HTTP API: http://localhost:${PORT}`);
    console.log(`\nAvailable endpoints:`);
    console.log(`  POST /command - Send command to browser`);
    console.log(`  GET /status - Check server status`);
    console.log(`\nExample command:`);
    console.log(`  curl -X POST http://localhost:${PORT}/command \\`);
    console.log(`    -H "Content-Type: application/json" \\`);
    console.log(`    -d '{"action": "runSimulation", "params": {}}'`);
    console.log(`\nWaiting for connections...\n`);
});

// Graceful shutdown
process.on('SIGINT', () => {
    console.log('\nShutting down remote control server...');
    wss.close(() => {
        server.close(() => {
            process.exit(0);
        });
    });
});