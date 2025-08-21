# Phased Re-implementation Plan for Solana

This document outlines a phased rollout strategy for implementing the Feels model as a Solana program. The approach prioritizes a robust, secure, and useful initial launch while ensuring a clear and non-disruptive path for introducing more advanced features like lending and leverage over time.

## Phase 1: The Concentrated Liquidity AMM with Feels Token System

The goal of Phase 1 is to launch a capital-efficient Automated Market Maker (AMM) for simple token swaps, built on a universal token wrapper system. This is achieved by:
1. Simplifying the 3D protocol model into a single dimension: **rate/price**
2. Standardizing all tokens through the Feels wrapper system (all pools use Token-2022 exclusively)

### 1.1. Core Simplification

-   **Duration Dimension:** Hardcoded to `Swap`. All liquidity and trades are for immediate settlement.
-   **Leverage Dimension:** Hardcoded to `Senior (1x)`. The concept of risk tranching is not implemented in this phase.
-   **Resulting Model:** A 1D concentrated liquidity AMM, functionally similar to Uniswap v3, but with all tokens wrapped in the Feels standard.

### 1.2. Feels Token Wrapper System

Every token entering the protocol is wrapped into a standardized Token-2022 representation:

-   **Standardization:** All pools exclusively use Token-2022 Feels-wrapped tokens (e.g., FeelsSOL, FeelsUSDC)
-   **Virtual Balances:** The wrapper system enables precise virtual balance tracking
-   **Middleware Layer:** Token-specific features (yield tracking, price feeds, etc.) are handled at the wrapper level
-   **Existing Example:** The JitoSOL → FeelsSOL system already demonstrates this pattern

### 1.3. Solana On-Chain Architecture (V1)

The key to this phased approach is designing the on-chain accounts to be upgradable. This is primarily achieved by **reserving extra space** in the main `Pool` account from day one.

#### `FeelsToken` Account

A standardized wrapper for all tokens entering the protocol:

-   `underlying_mint: Pubkey`: Original token mint (SPL or Token-2022)
-   `feels_mint: Pubkey`: Token-2022 Feels representation 
-   `token_type: TokenType`: Classification (NativeSOL, LiquidStaking, StandardSPL, etc.)
-   `price_tracker: PriceTracker`: Middleware for price feeds
-   `total_wrapped: u128`: Total amount of underlying tokens wrapped
-   `virtual_reserves: u128`: Virtual balance for the AMM
-   `middleware_config: [u8; 128]`: Extensible middleware configuration

#### `Pool` Account

This account will store the global state for a single trading pair of Feels-wrapped tokens.

-   `version: u8`: Initialized to `1`. This field is critical for managing future upgrades.
-   `token_a_mint: Pubkey`: The Feels-wrapped token A mint (e.g., FeelsUSDC).
-   `token_b_mint: Pubkey`: The Feels-wrapped token B mint (e.g., FeelsSOL).
-   `token_a_wrapper: Pubkey`: Reference to FeelsToken account for token A.
-   `token_b_wrapper: Pubkey`: Reference to FeelsToken account for token B.
-   `fee_rate: u16`: The fee tier for the pool (in basis points).
-   `current_tick: i32`: The current price tick index (a single integer in V1).
-   `current_sqrt_price: u128`: The current Q64.96 square root of the price.
-   `liquidity: u128`: The total active liquidity in the pool.
-   `virtual_reserves_a: u128`: Virtual balance for token A.
-   `virtual_reserves_b: u128`: Virtual balance for token B.
-   `_reserved: array[u8; 512]`: **Crucial for upgradability.** This is 512 bytes of empty space that will be repurposed in Phase 2 for new fields.

#### `Tick` Account

A Program Derived Address (PDA) keyed by its `tick_index` (`i32`).

-   `liquidity_net: i128`: The net change in liquidity when this tick is crossed.
-   `liquidity_gross: u128`: The total liquidity referencing this tick.
-   (Other fields for tracking fees).

#### `Position` NFT

A standard SPL Non-Fungible Token representing a user's liquidity position.

-   On-chain metadata for the NFT will store:
    -   `pool_id: Pubkey`
    -   `tick_lower: i32`
    -   `tick_upper: i32`
    -   `liquidity: u128`

### 1.4. V1 Program Instructions

The initial instruction set includes both token wrapper operations and AMM functionality:

#### Token Wrapper Instructions
-   `create_feels_token`: Deploy a new Feels wrapper for a token
-   `wrap_tokens`: Convert underlying tokens to Feels representation
-   `unwrap_tokens`: Redeem Feels tokens for underlying assets
-   `update_price_feed`: Update price tracking for wrapped tokens (permissioned)

#### AMM Instructions
-   `initialize_pool`: Create a new pool (only accepts Feels-wrapped Token-2022 tokens)
-   `create_position_nft`: Mint NFT representing a liquidity position
-   `add_liquidity`: Provide liquidity to a pool
-   `remove_liquidity`: Withdraw liquidity from a pool
-   `swap`: Execute token swap (with automatic virtual balance updates)
-   `collect_fees`: Claim accumulated trading fees

This V1 launch results in a complete, valuable, and standalone product: a capital-efficient DEX with standardized token handling and enhanced virtual balance capabilities.

---

## Phase 2: Activating the 3rd Dimension (Lending & Leverage)

In this phase, we use the architectural hooks established in V1 (versioning and reserved space) to introduce the more advanced protocol features without requiring a disruptive migration. The Feels token wrapper system provides the foundation for tracking term-based positions and leverage.

### 2.1. Enhanced Feels Token System

The wrapper system evolves to support multi-dimensional positions:

-   **Term Tracking:** Feels tokens can now represent term-locked positions (e.g., FeelsUSDC-30D)
-   **Leverage Tiers:** Wrapper tracks risk tranching (Senior/Junior positions)
-   **Yield Accumulation:** Automatic tracking of lending yields and fee distributions
-   **Virtual Balance Evolution:** Support for time-weighted and risk-adjusted virtual balances

### 2.2. The Upgrade Path

1.  **New Program Deployment:** A new version of the Solana program is deployed via `solana program deploy`. This new program binary contains the logic for both V1 and V2 pools.
2.  **`upgrade_pool` Instruction:** A new, permissioned instruction is introduced. When called by the pool's authority, it will:
    1.  Target a V1 `Pool` account.
    2.  Verify `pool.version == 1`.
    3.  Deserialize the V1 data structure.
    4.  Create the V2 data structure in memory, populating it with the old data and initializing the new fields.
    5.  Serialize the V2 structure back into the *same account*, overwriting the `_reserved` space.
    6.  Atomically set `pool.version = 2`.
3.  **Wrapper Enhancement:** Existing Feels tokens are upgraded to support the new dimensions without requiring unwrapping.

### 2.3. Evolved On-Chain Architecture (V2)

The `Pool` account's data structure is expanded into the previously reserved space.

-   **`Pool` Account (V2) Fields:**
    -   (All V1 fields remain).
    -   The `_reserved` space is now formally structured to hold:
        -   `dimension_weights: { wr: u64, wd: u64, wl: u64 }`
        -   `risk_params: { alpha_senior: u64, alpha_junior: u64 }`
        -   `total_senior_liquidity: u128`
        -   `total_junior_liquidity: u128`
        -   `term_state: TermTracker` - Tracks active term positions
        -   `yield_accumulator: YieldState` - Manages yield distribution

### 2.4. Evolved Logic

-   **3D Ticks:** The concept of a "tick" is expanded from a single `i32` to a 3D tuple: `(rate_tick, duration_tick, leverage_tick)`. The program logic now handles this multi-dimensional space. Existing V1 `Tick` accounts are interpreted by the program as having `duration = Swap` and `leverage = Senior`.

-   **Enhanced Feels Token Operations:**
    -   Term-based wrapping: `wrap_tokens_with_term` creates FeelsTokens with specific maturity dates
    -   Risk-tiered positions: `wrap_tokens_with_leverage` creates Senior/Junior Feels positions
    -   Automatic yield tracking: All Feels tokens accumulate yields based on their parameters

-   **Enhanced Instructions:**
    -   `add_liquidity` is updated to accept a full 3D `TickCoordinate` for its range.
    -   `swap` is updated to accept a 3D `TickCoordinate`, allowing users to specify their desired path (e.g., a token swap, a monthly loan, or a flash loan).
    -   `borrow` creates term-based Feels tokens representing loan positions
    -   `repay` burns term-based Feels tokens and releases collateral

-   **New Instructions:** 
    -   `claim_matured_position`: Redeem matured term positions
    -   `rollover_position`: Extend term positions
    -   `liquidate_junior`: Process junior tranche liquidations
    -   `distribute_yield`: Allocate accumulated yields to Feels token holders

-   **Risk Waterfall:** The core logic for `swap` and `remove_liquidity` is enhanced to include the redenomination and loss distribution mechanics defined in the original model, using the new on-chain risk parameter fields. The Feels token wrapper automatically handles risk-adjusted virtual balances.

### 2.5. Benefits of the Feels Token Architecture

-   **Unified Interface:** All complexity is handled at the wrapper layer, keeping the AMM logic clean
-   **Backward Compatibility:** V1 Feels tokens continue to work seamlessly
-   **Composability:** Other protocols can integrate with standardized Feels tokens
-   **Virtual Balance Precision:** Perfect tracking of all dimensions through the wrapper system

This phased strategy provides a secure and logical progression, allowing the protocol to build a user base and establish trust with a solid foundational product before introducing its more novel and complex features. The Feels token wrapper system ensures consistency across all protocol operations while enabling sophisticated multi-dimensional trading.
