# Feels Prototype

Feels is a DeFi system that unifies spot trading, lending, and leverage into a single tick-based market. Built in PureScript, this implementation serves as a functional prototype exploring how different monetary functions can be combined through composable position parameters.

## Overview

The protocol treats every position as a "tick" with three independent parameters: price level (tickRate), time lock (duration), and leverage multiplier (leverageRate). Different combinations of these parameters create different financial instruments, from simple spot trades to complex leveraged lending positions.

At its core, the system accepts jitoSOL deposits through a separate deposit layer, minting synthetic SOL that serves as the base currency for all market operations. This design isolates exchange rate risk while allowing users to maintain exposure to staking yields throughout their participation in the protocol.

## Architecture

The codebase follows a modular architecture that separates the deposit layer, unified tick market, and position management systems. The deposit component handles jitoSOL conversions and synthetic SOL minting, while the pool module implements the core AMM logic with concentrated liquidity support. Position creation and management are handled through a unified interface that allows arbitrary combinations of the three core parameters.

Risk assessment and fee calculation form a critical part of the system, implementing a triangular dual risk mitigation framework. Each position type carries specific risks—liquidity positions face impermanent loss, lending positions face credit risk, and leveraged positions face amplified price risk. The fee model dynamically adjusts based on system health metrics and the complexity of positions.

## Usage

### Development
- `nix develop` - Enter development shell
- `just build` - Build application (dev version with proxy system)
- `just serve` - Serve with log mirroring on port 9000
- `just serve-simple` - Serve without log mirroring

### WebSocket Control (Development)
- `just ws-server` - Start WebSocket server (port 3002)
- `just ping` - Test WebSocket connection
- `just sim` - Trigger simulation in running app
- `just ws-cmd <action>` - Send custom command

### Production
- `just build-prod` - Build for production (no proxy system)
- `just deploy` - Deploy to server (requires .env config)
