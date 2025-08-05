# WebSocket Control & Logging System

This directory contains the development proxy system for remote control and log mirroring of the Feels Protocol application.

## Overview

The proxy system provides bilateral WebSocket communication between the browser application and development tools, enabling:

- **Remote Control**: Execute commands in the running application from CLI/HTTP
- **Log Mirroring**: Mirror browser console logs to a web interface and file
- **Command Callbacks**: Receive responses from executed commands

## Components

### `websocket-server.js`
Main WebSocket server providing HTTP API and WebSocket bridge.

**Start the server:**
```bash
just ws-server
```

**Endpoints:**
- WebSocket: `ws://localhost:3002`
- HTTP API: `http://localhost:3002`
- POST `/command` - Send command to browser
- GET `/status` - Check server status

**Example HTTP command:**
```bash
curl -X POST http://localhost:3002/command \
  -H "Content-Type: application/json" \
  -d '{"action": "runSimulation", "params": {}}'
```

### `websocket-cli.js`
Command-line interface for sending commands to the WebSocket server.

**Usage:**
```bash
just ping
just sim  
just ws-cmd <action>
```

### `log-server.js`
Express server that mirrors browser console logs to a web interface and saves them to `logs/browser-console.log`.

**Features:**
- Web interface at `http://localhost:3001`
- Real-time log streaming
- File logging to `logs/browser-console.log`
- Integrated with `just serve` command

## Development Workflow

### Standard Development (with logging)
```bash
just serve
```
This automatically starts:
1. Log mirror server on port 3001
2. Main application on port 9000
3. Saves logs to `logs/browser-console.log`

### WebSocket Control Session
1. Start the main application:
   ```bash
   just serve
   ```

2. In another terminal, start WebSocket server:
   ```bash
   just ws-server
   ```

3. Send commands:
   ```bash
   just ping
   just sim
   just ws-cmd <action>
   ```

### Available Commands
- `just ping` - Test connectivity (returns "pong")
- `just sim` - Trigger simulation in the UI
- `just ws-cmd <action>` - Send custom command
- Custom actions registered via `registerRemoteAction()`

## Production Build

For production deployment **without** the proxy system:

```bash
just build-prod
```

This builds the application using `index.production.html` which excludes:
- `websocket-client.js`
- `log-client.js`
- All WebSocket/logging functionality

The production build is clean and contains only the core application code.

## Application Integration

- `assets/websocket-client.js` - Browser WebSocket client (dev only)
- `assets/log-client.js` - Browser console interceptor (dev only)
- `assets/index.html` - Development HTML (includes proxy scripts)
- `assets/index.production.html` - Production HTML (excludes proxy scripts)

## Ports

- **3001**: Log mirror web interface
- **3002**: WebSocket server (HTTP API + WebSocket)
- **9000**: Main application

## Troubleshooting

- **Port conflicts**: Kill existing processes with `just clean`
- **WebSocket connection issues**: Check browser console for connection errors
- **Command not working**: Verify WebSocket server is running and browser is connected
