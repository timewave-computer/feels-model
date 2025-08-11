# Gateway API Refinement

## Overview

The Gateway module has been refined to provide a cleaner, higher-level API that encapsulates internal implementation details while exposing only the necessary operations.

## API Changes

### Before (Exposed Internal Details)
```purescript
module Gateway
  ( -- ... other exports
  -- Re-export synthetic SOL functions
  , validateCollateral      -- Internal validation
  , getOraclePrice          -- Query function
  , mintFeelsSOL            -- Low-level mint operation
  , burnFeelsSOL            -- Low-level burn operation
  , getCollateralValue      -- Internal calculation
  , getTotalSupply          -- Query function
  , getCollateralRatio      -- Internal metric
  )
```

### After (Clean High-Level API)
```purescript
module Gateway
  ( -- ... other exports
  -- High-level operations
  , enterSystem             -- JitoSOL → FeelsSOL conversion
  , exitSystem              -- FeelsSOL → JitoSOL conversion
  -- High-level query functions
  , getOraclePrice          -- Current exchange rate data
  , getTotalSupply          -- Total FeelsSOL in circulation
  , getSystemHealth         -- Comprehensive health metrics
  )
```

## Key Improvements

### 1. **Encapsulated Internal Operations**
- `mintFeelsSOL` and `burnFeelsSOL` are now internal implementation details
- External modules use `enterSystem` and `exitSystem` for all gateway operations
- This enforces proper fee handling and accounting at the gateway boundary

### 2. **Simplified Query Interface**
- Removed direct access to `validateCollateral`, `getCollateralValue`, and `getCollateralRatio`
- Added `getSystemHealth` which provides a high-level view:
  ```purescript
  { collateralRatio :: Number
  , totalLocked :: Number
  , totalMinted :: Number
  , isHealthy :: Boolean
  }
  ```

### 3. **Clear Separation of Concerns**
- **Gateway Module**: Handles all JitoSOL ↔ FeelsSOL conversions and system health
- **Internal Functions**: Manage collateral validation, minting/burning mechanics
- **External Interface**: Provides only essential operations and queries

## Usage Examples

### Entering the System
```purescript
result <- enterSystem gateway "user123" 100.0
-- Handles: validation, minting, fees, POL contribution
```

### Checking System Health
```purescript
health <- getSystemHealth gateway
when (not health.isHealthy) $ 
  log "Warning: System under-collateralized"
```

## Benefits

1. **Reduced API Surface**: Fewer functions to understand and maintain
2. **Better Abstraction**: Implementation details hidden from consumers
3. **Enforced Invariants**: All operations go through proper channels
4. **Future Flexibility**: Internal implementation can change without breaking external code