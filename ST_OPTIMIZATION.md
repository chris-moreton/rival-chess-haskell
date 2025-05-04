# ST Monad Optimization for Chess Search

This branch implements a key performance optimization for the chess engine by using mutable state in the search algorithm to reduce memory allocations.

## The Problem

In the standard alpha-beta search algorithm, we:

1. Generate all possible moves from the current position
2. For each move:
   - Create a **new Position** by applying the move
   - Recursively search from the new position
   - Use the results to update alpha/beta bounds

Creating a new `Position` record for each move in the search tree is expensive because:

- It requires memory allocation for each new position
- It creates significant garbage collection pressure
- It reduces cache efficiency with constant object creation

## The Solution: ST Monad

The ST (State Thread) monad allows us to use mutable state in a controlled, isolated way. This implementation:

1. Maintains a **single mutable Position** that's modified in-place during search
2. Uses "make/unmake" move operations instead of creating new positions
3. Stores just enough information to restore the position after searching

Key components:

- `STRef s Position` - A mutable reference to the position
- `makeMoveST` - Apply a move to the position in-place
- `unmakeMoveST` - Restore the position to its state before the move
- `UndoInfo` - Data structure to store the state needed for undoing moves

## Performance Benefits

This approach typically provides:

- 2-3x reduction in memory allocations
- 1.5-3x speedup for deep searches
- Reduced garbage collection pauses

## Code Overview

- `STSearch.hs` - The main implementation of the ST-based search
  - `searchST` - Entry point for the ST-based search
  - `makeMoveST`, `unmakeMoveST` - Core move-handling functions
  - `searchInnerST` - The main alpha-beta search function

- `STSearchTest.hs` - Benchmark comparing regular search vs ST search

## How to Test

Run the benchmark to compare performance between the original and optimized versions:

```bash
stack build st-search-test
stack exec st-search-test -- [depth]
```

Where `[depth]` is an optional parameter for search depth (default: 6).

## Implementation Notes

1. The ST monad ensures the mutable state cannot "escape" and preserves referential transparency
2. Move generation still uses immutable data structures
3. This implementation focuses on reducing allocation overhead in the critical search path
4. Transposition table lookups are simplified in this implementation but would be fully integrated in final version