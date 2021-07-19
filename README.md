# Haskell Chess Functions

Running this code as an application will launch a UCI chess engine.

Positions are represented as a collection of bitboards and move generation is performed using bitboard manipulation, including magic bitboards. I generated the magic numbers by brute force for Rival Chess in its Java incarnation 15 or so years ago.

Example of UCI usage:

From start position

    position startpos       
    go depth 3

    info score cp 0 pv c2c4 b8c6 c4c5
    bestmove c2c4

Start position with move list and time limit in milliseconds

    position startpos moves e2e4 d7d5
    go movetime 1000

    info score cp 0 pv e4d5
    info score cp 0 pv e4d5 d8d5
    info score cp 0 pv e4d5 d8d5 c2c4
    info score cp 0 pv e4d5 d8d5 c2c4 d5c6
    info score cp 0 pv e4d5 d8d5 c2c4 d5c6 d2d4
    info score cp 0 pv e4d5 d8d5 c2c4 d5c6 d2d4 c8e6
    bestmove e4d5

From FEN - this one is a mate in five

    position fen 6k1/3b3r/1p1p4/p1n2p2/1PPNpP1q/P3Q1p1/1R1RB1P1/5K2 b - - 0 1
    go infinite

    info score cp -1200 pv h4f4
    info score cp -400 pv a5b4
    info score cp -400 pv a5b4 a3b4
    info score cp -400 pv a5b4 a3b4 c5b7
    info score cp 350 pv h4f4 f1e1 f4e3
    info score cp 500 pv h4f4 e2f3 f4e3 b4c5
    info score cp 600 pv h4f4 f1e1 f4e3 e1d1 a5b4
    info score cp 700 pv h4f4 e2f3 f4e3 f3h5 h7h5 b4c5
    info score cp 950 pv h4f4 e2f3 f4e3 f3h5 h7h5 d4f3 e4f3
    info score cp 9991 pv h4f4 e2f3 f4e3 f3h5 h7h5 d4f3 h5h1 f3g1

You can also add moves to a FEN

    position fen 2q1q1q1/8/1K6/8/7p/6kP/8/8 w - - 22 72 moves b6a5 g8f8 a5b6
    go depth 3

    info score cp 2800 pv c8h3
    info score cp 9999 pv f8c5
    info score cp 9999 pv f8c5
    bestmove f8c5

To exit the UCI application

    quit

## Some useful functions

Here are some of the functions used in the program. I've written them here as test cases showing the inputs and expected outputs.

### Get moves for a position (does not filter checks)

    describe "moves" $ do
        it "Gets all moves for a position" $ do
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

Various things brought me to my currently love of functional programming - firstly, the introduction of functional elements in Java and more comprehensively in Kotlin, and then my desire to learn Haskell due to interest in Cardano ADA. 

I used this project to learn Haskell. The first functions were written with no knowledge of the language, and the program grew from there. Initial functions where revisited as my understanding of the language progressed.

