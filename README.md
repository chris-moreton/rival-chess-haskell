# Haskell Chess Functions

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

Rival Chess was first written in Pascal in 1988, then C++ in 1994. It was converted to Java in 2010, then to Kotlin in 2019.

The Kotlin conversion moved the code towards a functional style, having been inspired by Cardano. I'm now rewriting it in Haskell and being forced to do it properly.
