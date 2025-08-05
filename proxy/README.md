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
node proxy/websocket-server.js
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
node proxy/websocket-cli.js <action> [params...]
node proxy/websocket-cli.js ping
node proxy/websocket-cli.js runSimulation
```

### `log-mirror-server.js`
Express server that mirrors browser console logs to a web interface and saves them to `browser.log`.

**Features:**
- Web interface at `http://localhost:3001`
- Real-time log streaming
- File logging to `browser.log`
- Integrated with `just serve` command

## Development Workflow

### Standard Development (with logging)
```bash
just serve
```
This automatically starts:
1. Log mirror server on port 3001
2. Main application on port 9000
3. Saves logs to `browser.log`

### WebSocket Control Session
1. Start the main application:
   ```bash
   just serve
   ```

2. In another terminal, start WebSocket server:
   ```bash
   node proxy/websocket-server.js
   ```

3. Send commands via CLI:
   ```bash
   node proxy/websocket-cli.js ping
   node proxy/websocket-cli.js runSimulation
   ```

### Available Commands
- `ping` - Test connectivity (returns "pong")
- `runSimulation` - Trigger simulation in the UI
- Custom actions registered via `registerRemoteAction()`

## Production Build

For production deployment **without** the proxy system:

```bash
just build-production
```

This builds the application using `index.production.html` which excludes:
- `remote-control-client.js`
- `log-mirror.js`
- All WebSocket/logging functionality

The production build is clean and contains only the core application code.

## Files Integration

- `assets/remote-control-client.js` - Browser WebSocket client (dev only)
- `assets/log-mirror.js` - Browser console interceptor (dev only)
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