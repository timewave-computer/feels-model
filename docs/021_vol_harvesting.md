# Volatility Harvesting Strategy

## Overview

Volatility Harvesting is a strategy employed by market participants ("Vol Harvesters") who aim to profit from market volatility by committing their FeelsSOL to the protocol for a 28-day rollover term. Their goal is to receive compensation for the inherent costs of adverse selection (Loss Versus Rebalancing, LVR) and to be eligible for additional volatility yield, without directly engaging in complex delta-neutral hedging strategies.

## The Core Strategy: FeelsSOL Commitment

Vol Harvesters commit their FeelsSOL to the protocol for a fixed 28-day term, with an option for automatic rollover. This commitment makes their FeelsSOL available to the protocol for its internal volatility monetization strategies.

*   **Mechanism**: Users create a position by committing a specified amount of FeelsSOL. This FeelsSOL is then managed by the protocol for a 28-day period. The position can be configured to automatically roll over for subsequent 28-day terms.
*   **Profit Source**: Vol Harvesters receive two forms of compensation from the protocol:
    1.  **LVR Compensation**: A payment designed to compensate for the estimated Loss Versus Rebalancing (LVR) incurred by the protocol's underlying volatility monetization activities.
    2.  **Volatility Yield**: An additional yield component, representing upside gains from the protocol's successful monetization of volatility beyond the LVR compensation.

## The Challenge: Protocol-Absorbed Loss Versus Rebalancing

While the protocol's internal volatility monetization activities (e.g., providing liquidity in AMMs) incur Loss Versus Rebalancing (LVR), the Vol Harvester is shielded from directly bearing this cost. Instead, the protocol absorbs the LVR and compensates the Vol Harvester through a dedicated payment.

### What is LVR in Practice?

LVR represents the systematic losses that liquidity providers incur from adverse selection - being forced to trade at stale AMM prices while arbitrageurs trade against them at more favorable rates. Unlike impermanent loss, LVR isolates the true economic cost of providing liquidity.

*   **Adverse Selection Cost**: When prices move on external markets (like centralized exchanges), AMM prices lag behind, creating arbitrage opportunities. Arbitrageurs exploit these stale quotes, causing LPs to systematically buy high and sell low.

*   **Always a Cost**: Unlike impermanent loss which can be positive or negative depending on price paths, LVR is monotonically increasing - it represents a real, measurable cost that LPs pay to arbitrageurs.

### LVR in Tick-Based Systems (Protocol's Perspective)

*   **Concentrated Liquidity Amplification**: 
    *   Higher liquidity concentration leads to steeper price curves (higher marginal liquidity $|x'(P)|$)
    *   This increases the instantaneous LVR rate: $\ell(\sigma, P) = \frac{\sigma^2 P^2}{2} |x'(P)|$
    *   Concentrated positions experience higher LVR per dollar but earn proportionally higher fees

*   **Range Dynamics**: 
    *   Within active ranges: LPs face continuous adverse selection as prices fluctuate
    *   At range boundaries: LPs experience the full force of adverse selection as positions flip between assets
    *   Outside ranges: Single-sided positions avoid further LVR but miss fee opportunities

### The LVR vs. Fees Trade-off (Protocol's Perspective)

The fundamental equation for LP profitability after hedging market risk is:
$\text{Delta-Hedged LP Returns} = \text{Trading Fees} - \text{LVR}$

*   **Fee Income**: Earned from legitimate traders and arbitrageurs passing through the position
*   **LVR Cost**: Paid to arbitrageurs exploiting stale prices
*   **Volatility Dependency**: Both fees and LVR increase with volatility, but LVR grows faster ($\propto \sigma^2$)




## Benefits for Vol Harvesters

By committing FeelsSOL to the protocol for volatility monetization, Vol Harvesters can:

*   **Simplicity**: Engage in volatility harvesting without directly managing complex AMM LP positions or hedging strategies.
*   **Guaranteed LVR Compensation**: Receive a payment from the protocol designed to offset the estimated LVR costs, providing a more predictable return profile.
*   **Eligibility for Volatility Yield**: Participate in the upside gains from the protocol's successful volatility monetization.
*   **Capital Efficiency**: Their committed FeelsSOL is utilized by the protocol, potentially generating returns that exceed simple holding.
*   **Focus on Core Bet**: Focus purely on the protocol's ability to monetize volatility, rather than managing individual LP risks.

### Strategic Advantages

This approach transforms volatility harvesting into a simplified, protocol-managed offering:

*   **Risk Abstraction**: Vol Harvesters are shielded from the direct complexities and risks of AMM LPing and hedging.
*   **Predictable Returns**: The LVR compensation provides a more predictable baseline return, making the strategy more attractive.
*   **Scalability**: The protocol can manage pooled FeelsSOL for volatility monetization at scale, benefiting all participating Vol Harvesters.

This strategy allows Vol Harvesters to achieve their motivation of profiting from volatility in a simplified, protocol-managed manner.
