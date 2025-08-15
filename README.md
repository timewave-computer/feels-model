# Feels Prototype

Feels is a DeFi system that unifies spot trading, lending, and exposure into a single tick-based market. Built in PureScript, this implementation serves as a functional prototype exploring how different monetary functions can be combined through composable position parameters.

## Overview

The protocol treats every position as a `Position` defined by parameters such as `TermCommitment` (for duration/time lock) and `Tranche` (for exposure/leverage). Price levels are managed by the underlying Automated Market Maker (AMM) pool. Different combinations of these parameters create various financial instruments, from simple spot trades to time-locked lending and leveraged positions.

At its core, the system accepts jitoSOL deposits through a separate deposit layer, minting FeelsSOL (synthetic SOL) that serves as the base currency for all market operations. This design isolates exchange rate risk while allowing users to maintain exposure to staking yields throughout their participation in the protocol.

## Architecture

The codebase follows a modular architecture that separates the deposit layer, unified tick market, and position management systems. The deposit component handles jitoSOL conversions and FeelsSOL minting, while the pool module implements the core AMM logic with concentrated liquidity support. Position creation and management are handled through a unified interface that allows various combinations of term and exposure parameters.

Risk assessment and fee calculation are integral parts of the system. While a sophisticated dynamic fee model is planned, the current implementation includes basic fees and a solvency invariant check to ensure system health. Each position type carries specific risks—liquidity positions face impermanent loss, lending positions face credit risk, and leveraged positions face amplified price risk.

## Usage

### Development
- `nix develop` - Enter development shell
- `just build` - Build application (dev version with proxy system)
- `just serve` - Serve with log mirroring on port 9000 (logs to `logs/feels-browser-console.log`)
- `just serve-simple` - Serve without log mirroring

### WebSocket Control (Development)
- `just ws-server` - Start WebSocket server (port 3002)
- `just ping` - Test WebSocket connection
- `just sim` - Trigger simulation in running app
- `just ws-cmd <action>` - Send custom command

### Production
- `just build-prod` - Build for production (no proxy system)
- `just deploy` - Deploy to server (requires .env config)

### Logging
All logs are stored in the `logs/` directory at the project root with consistent naming:
- `feels-browser-console.log` - Browser console output from the application
- `feels-websocket-server.log` - WebSocket server logs (port 3002)
- `.feels-httpserver.pid` - HTTP server process ID
- `.feels-logserver.pid` - Log server process ID
- `.feels-watch-state.txt` - File watch state for development

To monitor logs in real-time:
```bash
tail -f logs/feels-browser-console.log    # Browser console output
tail -f logs/feels-websocket-server.log   # WebSocket server logs
```
