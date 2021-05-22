# Haskell Chess Functions

This repo provides a number of useful functions for playing with chess positions. You will find functions for:

1) Generating chess moves from a given position
2) Making a move from a given position to return a new position
3) Determining if a given colour is in check
4) Determining if a given square is attacked by a given colour
5) Converting FENs and algebraic moves to internal representations for further processing

The move generation and move making routines are, as far as can be reasonably determined by perft testing (counting the total number of positions in a full game tree from a given position to a given depth and checking them against known correct values), 100% correct.

Positions are represented as a collection of bitboards and move generation is performed using bitboard manipulation, including magic bitboards. I generated the magic numbers by brute force for Rival Chess in its Java incarnation 15 or so years ago. Both the bishop and rook tables can be made smaller if I get around to searching for some better numbers, or converting someone else's numbers to work with my board numbering (bit 63 = A8, bit 0 = H1).

In addition to the functions, which can be used on their own, the Main.hs app is gradually becoming a full-fledged UCI chess engine. It will play a legal game of chess on a UCI interface. Currently it only implements a basic negamax search with no pruning and no extensions. It understands only material values, checkmates, stalemates and the various draw situations, so it will play a passable game of beginner chess at depth 3, but when there is nothing doing, it will just shuffle pieces about.

However, by the time you read this, you probably won't be reading this, because it will have been updated to play a better game.

Next steps:

- Iterative deepening, most importantly so that timed moves can be used.
- Alpha-beta pruning in the negamax algorithm, so that searches greater than depth 3 can realistically be used.
- An evaluation function, so that it doesn't play h2h4 as its opening move.
- Extensions, quiescence, etc...

This isn't my first rodeo, so it should only take a couple of weeks.

Example of UCI usage:

From start position

    position startpos       
    go depth 3
    bestmove h2h4

    position startpos moves e2e4 d7d5
    go depth 3
    bestmove e4e5

From FEN - this one is a mate in three

    position fen r5rk/5p1p/5R2/4B3/8/8/7P/7K w K - 0 1
    go depth 1
    bestmove f6f7
    go depth 5
    bestmove f6a6
    
You can also add moves to a FEN

    position fen 2q1q1q1/8/1K6/8/7p/6kP/8/8 w - - 22 72 moves b6a5 g8f8 a5b6
    go depth 3
    bestmove f8c5

    quit

### Get moves for a position (does not filter checks)

    describe "moves" $ do
        it "Get all moves for a position" $ do
            sort (map algebraicMoveFromMove (moves (getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/3K1B2/1r2N2P/6r1 w - c6 0 1")))
                `shouldBe` [
                      "a5a6","a5b6"
                    , "b7a8b","b7a8n","b7a8q","b7a8r","b7b8b","b7b8n","b7b8q","b7b8r"
                    , "d3c2","d3c3","d3c4","d3d2","d3d4","d3e3","d3e4"
                    , "d5c6","d5d6","d5e6"
                    , "e2c1","e2c3","e2d4","e2g1","e2g3"
                    , "e7e8b","e7e8n","e7e8q","e7e8r"
                    , "f3e4","f3g2","f3g4","f3h1","f3h5"
                    , "f4a4","f4b4","f4c4","f4d4","f4e4","f4f5","f4f6","f4f7","f4f8","f4g4","f4h4"
                    , "h2h3","h2h4"
                ]

### Filter moves that leave the mover in check

By default, we don't filter moves that put the mover in check. It is generally more efficient to discover this information during the search
when calculating a move from a given position.

    describe "moves" $ do
        it "Gets all moves for a position" $ do
            let position = getPosition "4k3/8/6N1/4K3/8/8/8/8 b - - 0 1"
            let noChecks = filter (\x -> not (isCheck (makeMove position x) (mover position))) (moves position)
            sort (map algebraicMoveFromMove noChecks) `shouldBe` ["e8d7","e8d8","e8f7","e8f8"]

### Determine if a square is attacked by a given side

    describe "isSquareAttackedBy" $ do
        it "Determines if a given square is attacked by a given colour in a given position" $ do
            let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP4/5R2/1q3B2/4Nr1P/R3K2R w Q - 0 1"
            isSquareAttackedBy position (bitRefFromAlgebraicSquareRef "d1") Black `shouldBe` True

### Determine if given side is in check

    describe "isCheck" $ do
        it "Determines if the given side's king is attacked by at least one of the other side's pieces" $ do
            let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP4/5R2/1q3B2/4Nr1P/R3K2R w Q - 0 1"
            isCheck position White `shouldBe` False

### Make a move
   
    describe "makeMove" $ do
        it "Makes a move from a position and returns a new position" $ do
          makeMove (getPosition "2kr3R/pppp1p1p/2n1b3/2bn1q2/8/4p3/PPPP1PpP/RNBQK2R b KQ - 0 1")
                   (moveFromAlgebraicMove "g2g1q")
                      `shouldBe` (getPosition "2kr3R/pppp1p1p/2n1b3/2bn1q2/8/4p3/PPPP1P1P/RNBQK1qR w KQ - 0 2")

### Perft Test                      

Counts the total number of positions in a full-width, unpruned search tree from a given starting position and to a given depth. For example, for the starting position at depth 0, the result should be 20. At depth 1, the result should be 400.

    perft :: Position -> Int -> Int
    perft position depth =
        if depth == 0
            then length notInCheckPositions
            else sum (map (\x -> perft x (depth - 1)) notInCheckPositions)
        where newPositions = map (makeMove position) (moves position)
              notInCheckPositions = filter (\x -> not (isCheck x (mover position))) newPositions

### Background

Rival Chess was first written in Pascal in 1988, then C++ in 1994. It was converted to Java in 2008, then to Kotlin in 2019.

The Kotlin conversion moved the code towards a functional style, having been inspired by Cardano. I'm now rewriting it in Haskell and being forced to do it properly. I am a terrible programmer.
