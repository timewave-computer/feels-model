#!/usr/bin/env node

// Simple CLI to send commands to the WebSocket server
const http = require('http');

const args = process.argv.slice(2);

if (args.length === 0) {
    console.log('Usage: node websocket-cli.js <action> [params...]');
    console.log('Examples:');
    console.log('  node websocket-cli.js ping');
    console.log('  node websocket-cli.js runSimulation');
    console.log('  node websocket-cli.js getStats');
    process.exit(1);
}

const action = args[0];
const params = {};

// Parse simple key=value params
for (let i = 1; i < args.length; i++) {
    const arg = args[i];
    if (arg.includes('=')) {
        const [key, value] = arg.split('=', 2);
        params[key] = value;
    }
}

const postData = JSON.stringify({ action, params });

const options = {
    hostname: 'localhost',
    port: 3002,
    path: '/command',
    method: 'POST',
    headers: {
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(postData)
    }
};

console.log(`Sending command: ${action}`);
if (Object.keys(params).length > 0) {
    console.log(`Parameters:`, params);
}

const req = http.request(options, (res) => {
    console.log(`Status: ${res.statusCode}`);
    
    let body = '';
    res.on('data', (chunk) => {
        body += chunk;
    });
    
    res.on('end', () => {
        try {
            const response = JSON.parse(body);
            console.log('Response:', JSON.stringify(response, null, 2));
        } catch (e) {
            console.log('Response:', body);
        }
    });
});

req.on('error', (e) => {
    console.error('Error:', e.message);
    process.exit(1);
});

req.write(postData);
req.end();