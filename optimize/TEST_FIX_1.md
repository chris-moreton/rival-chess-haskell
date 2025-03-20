⏺ Let me explain the issue with the captureMoves test:

The problem was that the captureMoves function was trying to be too clever by using a potentialCaptureMoves function that attempted to generate only moves that might be captures. However, it included all pawn moves, not just pawn
capture moves, which could lead to inconsistencies.

I've fixed the issue by simplifying the captureMoves function to directly filter the complete list of moves using the isCapture predicate. This matches exactly with what the test expects: captureMoves position should be
equivalent to filter (isCapture position) (moves position).

The original implementation was likely trying to optimize for performance by avoiding the generation of non-capture moves upfront, but the implementation had a bug that included all pawn moves rather than just pawn captures.

The new code I added with generatePawnCaptureMoves is a more correct implementation for when you do want to optimize, but for now, the direct filter of all moves is the simplest fix to make the test pass.

For more complex chess engines, a hybrid approach is often used:
1. Generate only likely captures (without moves to empty squares)
2. Apply a filtering step to handle edge cases (like en passant)

This allows for better performance while still maintaining correctness.