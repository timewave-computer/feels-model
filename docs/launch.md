# Cascading Term-Based Batch Auction with Continuous Learning

## Overview

The Cascading Term-Based Batch Auction combines time-commitment preferences with continuous price discovery through internalized priority fee auctions. The system releases tokens in small batches across sequential commitment terms (weekly → daily → hourly → spot), where participants bid base price plus priority fees. Each batch auction immediately learns from the priority fees paid, updating the base price for the next batch. This creates rapid convergence to true market price while generating protocol revenue from the priority fees.

## Core Mechanism with Continuous Learning

Each batch auction processes bids and immediately updates the base price based on the priority fees paid. If winners pay high priority fees, it signals underpricing and the next batch's base price increases proportionally. This batch-to-batch learning creates exponential convergence rather than random price exploration.

```purescript
type BatchSequence =
  { currentBatch :: Int
  , basePrice :: Number         -- Updates after each batch
  , learningRate :: Number      -- How aggressively to adjust
  , feeHistory :: Array Number  -- Recent fee ratios for stability
  }

processAndLearn :: BatchAuction -> BatchSequence -> BatchSequence
processAndLearn auction sequence =
  let result = processBatch auction
      feeRatio = result.protocolRevenue / result.basePayment
      
      -- Next base price incorporates this batch's information
      adjustment = 1.0 + (feeRatio * sequence.learningRate)
      newBasePrice = sequence.basePrice * adjustment
      
      -- Adapt learning rate based on fee stability
      feeVariance = variance (sequence.feeHistory <> [feeRatio])
      newLearningRate = if feeVariance > 0.5 
                        then sequence.learningRate * 0.9
                        else min 0.8 (sequence.learningRate * 1.1)
      
  in sequence { basePrice = newBasePrice
              , learningRate = newLearningRate
              , feeHistory = takeLast 10 (sequence.feeHistory <> [feeRatio]) }
```

## Multi-Level Learning Hierarchy

The system learns at three levels simultaneously. Within each phase, every batch learns from the previous batch's priority fees. Between phases, each phase starts from the previous phase's discovered price plus a commitment premium. Across launches, the system builds a model of typical fee patterns and convergence rates.

This hierarchy dramatically accelerates price discovery. The weekly phase might start at 0.001 SOL knowing nothing, but after 10 batches with continuous learning, it could already be at 0.008 SOL. After 50 batches, it stabilizes around 0.042 SOL. The daily phase then starts at 0.050 SOL (20% premium) rather than blindly starting low, potentially finding its equilibrium in just 10-20 batches instead of 50.

## Example Convergence Pattern

Consider a weekly phase progression with continuous learning. Batch 1 starts at 0.001 SOL base price. Winners pay 400% priority fees, signaling severe underpricing. With a 0.5 learning rate, batch 2's base price jumps to 0.003 SOL. Winners now pay 100% fees, still high but improving. Batch 3 adjusts to 0.0045 SOL. By batch 5, fees drop to 5%, indicating price discovery. Without learning, finding this price might take 100+ random batches.

The daily phase benefits from this discovery, starting at 0.050 SOL. Its first batch might see only 20% priority fees, confirming the starting point was well-informed. Within 5-10 batches, daily phase converges to 0.055 SOL. The learning rate can be more aggressive here since we're fine-tuning rather than discovering.

## Dynamic Learning Rate Adaptation

The learning rate itself adapts based on signal quality. When priority fees are chaotic (high variance), the system learns conservatively to avoid overreacting to noise. When fees are consistent, learning accelerates. Early batches typically show high variance as the market discovers value, warranting slower learning. As consensus emerges, the system can adjust more aggressively.

```purescript
adaptLearningRate :: Array FeeRatio -> Number -> Number
adaptLearningRate recentFees currentRate =
  let consistency = 1.0 - (stdDev recentFees / mean recentFees)
  in if consistency > 0.8 then min 0.8 (currentRate * 1.2)
     else if consistency < 0.3 then max 0.1 (currentRate * 0.8)
     else currentRate
```

## Revenue Optimization Through Learning

Continuous learning maximizes protocol revenue by extracting the highest fees when price discovery is most uncertain. Early batches with low base prices generate massive priority fees (often 100-500% of base), creating substantial protocol revenue. As the system learns and base prices approach market value, priority fees naturally decrease to 5-10%, signaling successful discovery.

This creates aligned incentives: the protocol earns most when providing the most value (accurate price discovery), and earnings decrease as the market becomes efficient. A successful launch might generate 50% of its total fee revenue in the first 20% of batches, when information value is highest.

## Phase Transitions with Information

Each phase's learning directly informs the next phase's parameters. The weekly phase might process 100 batches, converging from 0.001 to 0.042 SOL with average priority fees dropping from 200% to 5%. This pattern reveals both the price level (0.042) and market confidence (low final fees). The daily phase starts at 0.050 SOL with smaller batches and higher learning rate, knowing it's already close to market value.

The hourly phase might start at 0.056 SOL with very aggressive learning (0.7 rate) since two phases of discovery have already occurred. It could converge in just 10-15 batches. By the spot market opening, the system has high confidence in the discovered price, having seen consistent low priority fees across multiple phases.

## Implementation Advantages

This continuous learning design is particularly efficient on Solana. Each batch processes as a single transaction that reads the previous batch's results and updates the base price atomically. No external oracles or complex calculations are needed - the priority fees themselves are the signal. The account model allows efficient storage of recent fee history for variance calculations and learning rate adjustments.

The system can even pre-compute likely base prices for upcoming batches based on current trends, allowing users to estimate their likely entry points. This transparency reduces anxiety while maintaining fair competition through the priority fee mechanism.

## Accelerated Convergence Metrics

With continuous learning, convergence is exponential rather than linear. A typical pattern might show 80% price discovery within the first 20 batches of weekly phase, 95% discovery by batch 50, and full convergence by batch 100. Without learning, the same discovery might require 500+ batches of random exploration. This efficiency means launches complete faster, users get tokens sooner, and the protocol processes fewer transactions.

The system tracks convergence through priority fee decay rates. When fees consistently stay below 10% of base price for 10 consecutive batches, the phase can confidently transition. This objective metric prevents both premature transitions (leaving value undiscovered) and excessive processing (wasting time after convergence).

## Conclusion

The continuous learning mechanism transforms batch auctions from isolated events into an interconnected discovery system. Each batch's priority fees immediately improve the next batch's pricing, creating exponential convergence to true market value. Combined with cascading term phases, this creates a highly efficient launch mechanism that discovers accurate prices in minimal batches while generating maximum protocol revenue when information value is highest. The system is self-improving, with each launch refining the model for faster convergence in future launches.