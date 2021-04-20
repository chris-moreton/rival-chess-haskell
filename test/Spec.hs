{-# LANGUAGE BinaryLiterals,NegativeLiterals #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Util.Bitboards
import Util.Fen
import Util.Utils
import Model.Game
import Search.MoveGenerator
import Types
import Data.Bits
import Data.Sort
import Search.MakeMove

main :: IO ()
main = hspec $ do
  describe "southFill" $ do
    it "South fills a bitboard" $ do
      southFill 4611936708517363882 `shouldBe` (4629952088967215103 :: Int)

  describe "northFill" $ do
    it "North fills a bitboard" $ do
      northFill 4611936708517363882 `shouldBe` (-1332566 :: Int)
 
  describe "rankBitboards" $ do
    it "Calculates rank8Bitboards - sanity check for values expressed as functions" $ do
      rank8Bits `shouldBe` (-72057594037927936)
      fileABits `shouldBe` (-9187201950435737472)
      fileHBits `shouldBe` 72340172838076673
      middleFiles8Bit `shouldBe` 24
      nonMidFiles8Bit `shouldBe` 231
      f1G1Bits `shouldBe` 6
      g1H1Bits `shouldBe` 3
      a1B1Bits `shouldBe` 192
      b1C1Bits `shouldBe` 96
      f8G8Bits `shouldBe` 432345564227567616
      g8H8Bits `shouldBe` 216172782113783808
      a8B8Bits `shouldBe` -4611686018427387904
      b8C8Bits `shouldBe` 6917529027641081856
      lightSquaresBits `shouldBe` -6172840429334713771
      darkSquaresBits `shouldBe` 6172840429334713770
      low32Bits `shouldBe` 4294967295

  describe "fenBoardPart" $ do
    it "Extracts board part from FEN" $ do
      fenBoardPart "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b - g3 5 56" `shouldBe` "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8"

  describe "fenRanks" $ do
    it "Extracts ranks from FEN board part from" $ do
      getFenRanks "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8" `shouldBe` ["6k1","6p1","1p2q2p","1p5P","1P3RP1","2PK1B2","1r2N3","8"]

  describe "rankBits" $ do
    it "Converts from FEN rank string into char array of eight 0s and 1s for a given piece" $ do
      rankBits "8" 'Q' `shouldBe` [0,0,0,0,0,0,0,0]
      rankBits "6k1" 'k' `shouldBe` [0,0,0,0,0,0,1,0]
      rankBits "6k1" 'q' `shouldBe` [0,0,0,0,0,0,0,0]
      rankBits "6p1" 'p' `shouldBe` [0,0,0,0,0,0,1,0]
      rankBits "6pp" 'p' `shouldBe` [0,0,0,0,0,0,1,1]
      rankBits "P7" 'P' `shouldBe` [1,0,0,0,0,0,0,0]
      rankBits "1p2q2p" 'p' `shouldBe` [0,1,0,0,0,0,0,1]

  describe "boardBits" $ do
    it "Converts from FEN string into char array of 64 0s and 1s for a given piece" $ do
      let fen = "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56"
      boardBits (getFenRanks (fenBoardPart fen)) 'p' `shouldBe` [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

  describe "pieceBitboard" $ do
    it "Converts from FEN rank string into char array of eight 0s and 1s" $ do
      let fen = "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b Q g3 5 56"
      pieceBitboard (getFenRanks (fenBoardPart fen)) 'p' `shouldBe` 634693087133696

  describe "algebraicSquareRefFromBitRef" $ do
    it "Converts a bitRef to an algebraic square" $ do
      algebraicSquareRefFromBitRef 63 `shouldBe` "a8"
      algebraicSquareRefFromBitRef 0 `shouldBe` "h1"
      algebraicSquareRefFromBitRef 7 `shouldBe` "a1"

  describe "bitRefFromAlgebraicSquareRef" $ do
    it "Converts an algebraic square to a bit reference" $ do
      bitRefFromAlgebraicSquareRef "a8" `shouldBe` 63
      bitRefFromAlgebraicSquareRef "h1" `shouldBe` 0
      bitRefFromAlgebraicSquareRef "g1" `shouldBe` 1

  describe "algebraicMoveFromMove" $ do
    it "Converts a compact move to an algebraic move" $ do
      algebraicMoveFromMove 458808 `shouldBe` "a1h8"
      map algebraicMoveFromMove [] `shouldBe` []

  describe "boardFromFen" $ do
    it "Converts from FEN to board type (Test 1)" $ do
      let fen = "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b q g3 5 56"
      let position = getPosition fen
      let bitboards = positionBitboards position
      whitePawnBitboard bitboards `shouldBe` 5404360704
      whiteKnightBitboard bitboards `shouldBe` 2048
      whiteKingBitboard bitboards `shouldBe` 1048576
      whiteBishopBitboard bitboards `shouldBe` 262144
      whiteQueenBitboard bitboards `shouldBe` 0
      whiteRookBitboard bitboards `shouldBe` 67108864
      blackPawnBitboard bitboards `shouldBe` 634693087133696
      blackKnightBitboard bitboards `shouldBe` 0
      blackKingBitboard bitboards `shouldBe` 144115188075855872
      blackBishopBitboard bitboards `shouldBe` 0
      blackQueenBitboard bitboards `shouldBe` 8796093022208
      blackRookBitboard bitboards `shouldBe` 16384
      mover position `shouldBe` Black
      enPassantSquare position `shouldBe` 17

    it "Converts from FEN to board type (Test 2)" $ do
      let fen = "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 w kQ - 5 56"
      let position = getPosition fen
      let castlePrivs = positionCastlePrivs position
      enPassantSquare position `shouldBe` -1
      halfMoves position `shouldBe` 5
      mover position `shouldBe` White
      whiteKingCastleAvailable castlePrivs `shouldBe` False
      whiteQueenCastleAvailable castlePrivs `shouldBe` True
      blackKingCastleAvailable castlePrivs `shouldBe` True
      blackQueenCastleAvailable castlePrivs `shouldBe` False

  describe "getPieceBitboardForColour" $ do
    it "Gets piece bitboard for colour" $ do
      let position = getPosition "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56"
      let bitboards = positionBitboards position
      bitboardForMover position Pawn `shouldBe` blackPawnBitboard bitboards
      bitboardForMover position Pawn `shouldNotBe` whitePawnBitboard bitboards
      bitboardForMover position Rook `shouldBe` blackRookBitboard bitboards
      bitboardForMover position Rook `shouldNotBe` whiteRookBitboard bitboards
      bitboardForMover position King `shouldBe` blackKingBitboard bitboards
      bitboardForMover position King `shouldNotBe` whiteKingBitboard bitboards
      bitboardForMover position Knight `shouldBe` blackKnightBitboard bitboards
      bitboardForMover position Knight `shouldNotBe` whiteKnightBitboard bitboards
      bitboardForMover position Queen `shouldBe` blackQueenBitboard bitboards
      bitboardForMover position Queen `shouldNotBe` whiteQueenBitboard bitboards
      bitboardForMover position Bishop `shouldBe` blackBishopBitboard bitboards
      bitboardForMover position Bishop `shouldNotBe` whiteBishopBitboard bitboards

  describe "bitRefList" $ do
    it "Gets a list of set bits in a bitboard" $ do
      let position = getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56"
      let bitboards = positionBitboards position
      bitRefList (blackKnightBitboard bitboards) `shouldBe` [46,49,63]

  describe "bitString" $ do
    it "Converts a bitboard to a string of 1s and 0s" $ do
      bitString 15 `shouldBe` "0000000000000000000000000000000000000000000000000000000000001111"

  describe "allBitsExceptFriendlyPieces" $ do
    it "Gets a bitboard with all bit set, except for friendly pieces" $ do
      let position = getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56"
      allBitsExceptFriendlyPieces position `shouldBe` 0b0111110111111101101101101011111111111111111111111011111111111111

  describe "allPiecesBitboard" $ do
    it "Gets a bitboard with bits set for all pieces" $ do
      let position = getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56"
      bitString (allPiecesBitboard position) `shouldBe` "1000001000000010010010010100000101000110001101000100100000000000"

  describe "emptySquaresBitboard" $ do
    it "Gets a bitboard with bits set for all empty squares" $ do
      let position = getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56"
      bitString (emptySquaresBitboard position) `shouldBe` "0111110111111101101101101011111010111001110010111011011111111111"
      
  describe "movesFromToSquares" $ do
    it "Creates a list of moves from a fromSquare and a list of toSquares" $ do
      movesFromToSquares 11 [22,33,44] `shouldBe` [720918,720929,720940]

  describe "generateKnightMoves" $ do
    it "Generates knight moves from a given FEN (ignoring checks)" $ do
      sort (map algebraicMoveFromMove (generateKnightMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56")))
        `shouldBe` ["a8c7","b6a4","b6c4","b6c8","b6d5","b6d7","g7e8","g7f5","g7h5"]
      sort (map algebraicMoveFromMove (generateKnightMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 w kQKq g3 5 56")))
        `shouldBe` ["e2c1","e2d4","e2g1","e2g3"]

  describe "generateKingMoves" $ do
    it "Generates king moves from a given FEN (ignoring checks)" $ do
      sort (map algebraicMoveFromMove (generateKingMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56")))
        `shouldBe` ["g8f7","g8f8","g8h7","g8h8"]
      sort (map algebraicMoveFromMove (generateKingMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 w kQKq g3 5 56")))
        `shouldBe` ["d3c2","d3c4","d3d2","d3d4","d3e3","d3e4"]

  describe "generateBishopMoves" $ do
    it "Generates bishop moves (including diagonal queen moves) from a given FEN (ignoring checks)" $ do
      sort (map algebraicMoveFromMove (generateBishopMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/7R w kQKq g3 5 56")))
        `shouldBe` ["f3a8","f3b7","f3c6","f3d5","f3e4","f3g2"]
      sort (map algebraicMoveFromMove (generateBishopMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56")))
        `shouldBe` ["e6a2","e6b3","e6c4","e6c8","e6d5","e6d7","e6f5","e6f7","e6g4"]

  describe "generateRookMoves" $ do
    it "Generates rook moves (including non-diagonal queen moves) from a given FEN (ignoring checks)" $ do
      sort (map algebraicMoveFromMove (generateRookMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 w kQKq g3 5 56")))
        `shouldBe` ["f4c4","f4d4","f4e4","f4f5","f4f6","f4f7","f4f8"]
      sort (map algebraicMoveFromMove (generateRookMoves (getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/6r1 b kQKq g3 5 56")))
        `shouldBe` ["b2a2","b2b1","b2b3","b2b4","b2c2","b2d2","b2e2","e6c6","e6d6","e6e2","e6e3","e6e4","e6e5","e6e7","e6e8","e6f6","e6g6","g1a1","g1b1","g1c1","g1d1","g1e1","g1f1","g1g2","g1g3","g1g4","g1h1"]

  describe "generatePawnMovesFromToSquares" $ do
    it "Creates a list of moves from a given From Square and a list of To Squares" $ do
      sort (map algebraicMoveFromMove (generatePawnMovesFromToSquares 54 [63,62,61]))
        `shouldBe` ["b7a8b","b7a8n","b7a8q","b7a8r","b7b8b","b7b8n","b7b8q","b7b8r","b7c8b","b7c8n","b7c8q","b7c8r"]
      sort (map algebraicMoveFromMove (generatePawnMovesFromToSquares 46 [55,54,53]))
        `shouldBe` ["b6a7","b6b7","b6c7"]

  describe "enemyBitboard" $ do
    it "Returns a bitboard with bits set for enemy pieces" $ do
      enemyBitboard (getPosition "n5k1/1P2P1n1/1n2q2p/Pp1pP3/3P1R2/3K1B2/1r2N2P/6r1 b - - 0 1")
        `shouldBe` 0b0000000001001000000000001000100000010100000101000000100100000000

  describe "pawnCaptures" $ do
    it "Returns a bitboard showing target squares for pawn captures, from a given square and an enemy piece bitboard" $ do
      pawnCaptures whitePawnMovesCapture 29 (enemyBitboard (getPosition "n5k1/1P4n1/1n2q2p/Pp3P2/3P1R2/3K1B2/1r2N2P/6r1 w - - 0 1"))
        `shouldBe` 0b0000000000000000000000000100000000000000000000000000000000000000
      pawnCaptures whitePawnMovesCapture 51 (enemyBitboard (getPosition "n5k1/1P2P1n1/1n2q2p/Pp1pP3/3P1R2/3K1B2/1r2N2P/6r1 w - - 0 1"))
        `shouldBe` 0b0000000000000000000000000000000000000000000000000000000000000000
      pawnCaptures whitePawnMovesCapture 54 (enemyBitboard (getPosition "n5k1/1P2P1n1/1n2q2p/Pp1pP3/3P1R2/3K1B2/1r2N2P/6r1 w - - 0 1"))
        `shouldBe` 0b1000000000000000000000000000000000000000000000000000000000000000
      pawnCaptures whitePawnMovesCapture 51 (enemyBitboard (getPosition "n5k1/4P1n1/1n2q2p/1p1p4/5R2/3K1B2/1r2N3/6r1 w - - 0 1"))
        `shouldBe` 0b0000000000000000000000000000000000000000000000000000000000000000

  describe "potentialPawnJumpMoves" $ do
    it "Returns a bitboard showing target squares for pawn moves that would land on the two-move rank if moved one more rank" $ do
      potentialPawnJumpMoves 0b0101000000000100010000011000000001000000010101010000001100010001 (getPosition "n5k1/1P2P1n1/1n2q2p/Pp6/3P1R2/3K1B2/1r2N2P/6r1 w - d5 0 1")
        `shouldBe` 0b0000000000000000000000000000000001010101000000000000000000000000

  describe "enPassantSquare position" $ do
    it "Identifies the enPassant square from a position" $ do
      enPassantSquare (getPosition "n5k1/4P1n1/4q2p/PpP1n3/3P1R2/3K1B2/1r2N2P/6r1 w - b6 0 1") `shouldBe` 46
      enPassantSquare (getPosition "n5k1/4P1n1/4q2p/PpP1n3/3P1R2/3K1B2/1r2N2P/6r1 w - - 0 1") `shouldBe` -1

  describe "pawnForwardAndCaptureMovesBitboard" $ do
    it "Returns a bitboard showing available landing squares (capture and non-capture) for a pawn on a given square" $ do
      let position = getPosition "n5k1/4P1n1/1n2q2p/1p1p4/5R2/3K1B2/1r2N3/6r1 w - - 0 1"
      let emptySquares = emptySquaresBitboard position
      emptySquares `shouldBe` 0b0111110111110101101101101010111111111011111010111011011111111101
      let fromSquare = 51
      let forwardMovesForSquare = whitePawnMovesForward!!fromSquare
      forwardMovesForSquare `shouldBe` 0b0000100000000000000000000000000000000000000000000000000000000000
      let pfmb = pawnForwardMovesBitboard ((Data.Bits..&.) forwardMovesForSquare emptySquares) position
      pfmb `shouldBe` 0b0000100000000000000000000000000000000000000000000000000000000000
      pawnForwardAndCaptureMovesBitboard fromSquare whitePawnMovesCapture pfmb position
        `shouldBe` 0b0000100000000000000000000000000000000000000000000000000000000000

  describe "generatePawnMoves" $ do
    it "Generates pawn moves from a given FEN (ignoring checks)" $ do
      sort (map algebraicMoveFromMove (generatePawnMoves (getPosition "n5k1/4P1n1/1n2q2p/1p1p4/5R2/3K1B2/1r2N3/6r1 w - - 0 1")))
        `shouldBe` ["e7e8b","e7e8n","e7e8q","e7e8r"]
      sort (map algebraicMoveFromMove (generatePawnMoves (getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/3K1B2/1r2N2P/6r1 w - c6 0 1")))
        `shouldBe` ["a5a6","a5b6","b7a8b","b7a8n","b7a8q","b7a8r","b7b8b","b7b8n","b7b8q","b7b8r","d5c6","d5d6","d5e6","e7e8b","e7e8n","e7e8q","e7e8r","h2h3","h2h4"]

  describe "anySquaresInBitboardAttacked" $ do
    it "Returns True if any squares set in the bitboard are attacked by the given attacker" $ do
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K1r1 w Q - 0 1"
      let bitboard = 0b0000000000000000000000000000000000000000010110000000000000000000
      anySquaresInBitboardAttacked position White bitboard `shouldBe` False
      let bitboard = 0b0000000000000000000000000000000000000000111110000000000000000000
      anySquaresInBitboardAttacked position White bitboard `shouldBe` True
      anySquaresInBitboardAttacked position White ((.|.) (1 `shiftL` 60) (1 `shiftL` 61)) `shouldBe` True
      anySquaresInBitboardAttacked position Black emptyCastleSquaresWhiteQueen `shouldBe` True
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K2R w Q - 0 1"
      anySquaresInBitboardAttacked position Black ((.|.) (1 `shiftL` 3) (1 `shiftL` 2)) `shouldBe` False
      anySquaresInBitboardAttacked position Black ((.|.) (1 `shiftL` 3) (1 `shiftL` 4)) `shouldBe` False
      let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP4/5R2/1q3B2/4Nr1P/R3K2R w Q - 0 1"
      anySquaresInBitboardAttacked position Black ((.|.) (1 `shiftL` 3) (1 `shiftL` 2)) `shouldBe` True
      anySquaresInBitboardAttacked position Black 0b0000000000000000000000000000000000000000000000000000000000011000 `shouldBe` True
      let position = getPosition "r3k2r/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K2R b Q - 0 1"
      isSquareAttackedBy position 60 White `shouldBe` True
      anySquaresInBitboardAttacked position White 0b0001100000000000000000000000000000000000000000000000000000000000 `shouldBe` True
      not (anySquaresInBitboardAttacked position White ((.|.) (1 `shiftL` 59) (1 `shiftL` 60))) `shouldBe` False

  describe "generateCastleMovesForMover" $ do
    it "Generates castle moves for a given mover" $ do
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K1r1 w Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black True True emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` []
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K2R w Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black True True emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` ["e1c1","e1g1"]
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/3rN2P/R3K2R w Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black True True emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` ["e1g1"]
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/4Nr1P/R3K2R w Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black True True emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` ["e1c1"]
      let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP4/5R2/1q3B2/4Nr1P/R3K2R w Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black True True emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` []
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black False True emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` []
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black True False emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` []
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 3 4 Black False False emptyCastleSquaresWhiteKing emptyCastleSquaresWhiteQueen (allPiecesBitboard position)))
        `shouldBe` []
      let position = getPosition "r3k1R1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K1r1 b Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White True True emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position))) `shouldBe` []
      let position = getPosition "r3k2r/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K2R b Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White True True emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position))) `shouldBe` []
      let position = getPosition "r3k2r/1P2PRn1/1n2q2p/P1pP4/8/5B2/1r2N2P/R3K2R b Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White True True emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position)))
        `shouldBe` []
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White False True emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position)))
        `shouldBe` []
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White True False emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position)))
        `shouldBe` []
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White False False emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position)))
        `shouldBe` []
      let position = getPosition "r3k2r/1P3Rn1/1n2q2p/P1pP2P1/8/5B2/1r2N2P/R3K2R b Q - 0 1"
      sort (map algebraicMoveFromMove (generateCastleMovesForMover position 59 60 White True True emptyCastleSquaresBlackKing emptyCastleSquaresBlackQueen (allPiecesBitboard position)))
        `shouldBe` ["e8c8"]

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
      sort (map algebraicMoveFromMove (moves (getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/3p1R2/2p2B2/1rPPN2P/R3K1r1 w Q - 0 1")))
        `shouldBe` [
              "a1a2","a1a3","a1a4","a1b1","a1c1","a1d1"
            , "a5a6","a5b6"
            , "b7a8b","b7a8n","b7a8q","b7a8r","b7b8b","b7b8n","b7b8q","b7b8r"
            , "d2c3","d2d3"
            , "d5d6","d5e6"
            , "e1d1","e1f1","e1f2"
            , "e2c1","e2c3","e2d4","e2g1","e2g3"
            , "e7e8b","e7e8n","e7e8q","e7e8r"
            , "f3e4","f3g2","f3g4","f3h1","f3h5"
            , "f4d4","f4e4","f4f5","f4f6","f4f7","f4f8","f4g4","f4h4"
            , "h2h3","h2h4"
         ]

  describe "isSquareAttackedBy" $ do
    it "Determines if a given square is attacked by a given colour in a given position" $ do
      let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP4/5R2/1q3B2/4Nr1P/R3K2R w Q - 0 1"
      anySquaresInBitboardAttacked position Black ((.|.) (1 `shiftL` 3) (1 `shiftL` 2)) `shouldBe` True
      isBishopAttackingSquare 4 22 (allPiecesBitboard position) `shouldBe` True
      isBishopAttackingSquare 5 22 (allPiecesBitboard position) `shouldBe` False
      isSquareAttackedBy position (bitRefFromAlgebraicSquareRef "d1") Black `shouldBe` True
      isSquareAttackedBy position 58 White `shouldBe` True
      isSquareAttackedBy position 60 White `shouldBe` True
      let position = getPosition "n5k1/1P2P1n1/1n2q2p/P1pP4/5R2/5B2/1r2N2P/R3K1r1 w Q - 0 1"
      isSquareAttackedBy position 0 White `shouldBe` True
      isSquareAttackedBy position 0 Black `shouldBe` True
      isSquareAttackedBy position 1 White `shouldBe` True
      isSquareAttackedBy position 1 Black `shouldBe` False
      isSquareAttackedBy position 2 White `shouldBe` True
      isSquareAttackedBy position 2 Black `shouldBe` True
      isSquareAttackedBy position 3 White `shouldBe` True
      isSquareAttackedBy position 3 Black `shouldBe` True
      isSquareAttackedBy position 4 White `shouldBe` True
      isSquareAttackedBy position 4 Black `shouldBe` False
      isSquareAttackedBy position 5 White `shouldBe` True
      isSquareAttackedBy position 5 Black `shouldBe` False
      isSquareAttackedBy position 6 White `shouldBe` True
      isSquareAttackedBy position 6 Black `shouldBe` True
      isSquareAttackedBy position 7 White `shouldBe` False
      isSquareAttackedBy position 7 Black `shouldBe` False
      isSquareAttackedBy position 8 White `shouldBe` False
      isSquareAttackedBy position 8 Black `shouldBe` False
      isSquareAttackedBy position 9 White `shouldBe` True
      isSquareAttackedBy position 9 Black `shouldBe` True
      isSquareAttackedBy position 10 White `shouldBe` True
      isSquareAttackedBy position 10 Black `shouldBe` False
      isSquareAttackedBy position 11 White `shouldBe` True
      isSquareAttackedBy position 11 Black `shouldBe` True
      isSquareAttackedBy position 12 White `shouldBe` True
      isSquareAttackedBy position 12 Black `shouldBe` True
      isSquareAttackedBy position 13 White `shouldBe` False
      isSquareAttackedBy position 13 Black `shouldBe` True
      isSquareAttackedBy position 14 White `shouldBe` False
      isSquareAttackedBy position 14 Black `shouldBe` False
      isSquareAttackedBy position 15 White `shouldBe` True
      isSquareAttackedBy position 15 Black `shouldBe` True
      isSquareAttackedBy position 16 White `shouldBe` False
      isSquareAttackedBy position 16 Black `shouldBe` True
      isSquareAttackedBy position 17 White `shouldBe` True
      isSquareAttackedBy position 17 Black `shouldBe` True
      isSquareAttackedBy position 18 White `shouldBe` True
      isSquareAttackedBy position 18 Black `shouldBe` False
      isSquareAttackedBy position 19 White `shouldBe` False
      isSquareAttackedBy position 19 Black `shouldBe` True
      isSquareAttackedBy position 40 White `shouldBe` False
      isSquareAttackedBy position 40 Black `shouldBe` True
      isSquareAttackedBy position 41 White `shouldBe` False
      isSquareAttackedBy position 41 Black `shouldBe` True
      isSquareAttackedBy position 42 White `shouldBe` True
      isSquareAttackedBy position 42 Black `shouldBe` True
      isSquareAttackedBy position 43 White `shouldBe` True
      isSquareAttackedBy position 43 Black `shouldBe` True
      isSquareAttackedBy position 44 White `shouldBe` False
      isSquareAttackedBy position 44 Black `shouldBe` True
      isSquareAttackedBy position 45 White `shouldBe` True
      isSquareAttackedBy position 45 Black `shouldBe` True
      isSquareAttackedBy position 61 White `shouldBe` True
      isSquareAttackedBy position 61 Black `shouldBe` True
      isSquareAttackedBy position 62 White `shouldBe` False
      isSquareAttackedBy position 62 Black `shouldBe` False
      isSquareAttackedBy position 63 White `shouldBe` True
      isSquareAttackedBy position 63 Black `shouldBe` True
                        
  describe "isCheck" $ do
    it "Determines if the given side's king is attacked by at least one of the other side's pieces" $ do
      let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP4/5R2/1q3B2/4Nr1P/R3K2R w Q - 0 1"
      isCheck position White `shouldBe` False
      isCheck position Black `shouldBe` False
      let position = getPosition "n4Rk1/1P2P1n1/1n5p/P1pP4/8/1q3B2/4Nr1P/R3K2R w Q - 0 1"
      isCheck position White `shouldBe` False
      isCheck position Black `shouldBe` True
      let position = getPosition "n4Rk1/1P2P1n1/1n5p/P1pP4/8/2q2B2/4Nr1P/R3K2R w Q - 0 1"
      isCheck position White `shouldBe` True
      isCheck position Black `shouldBe` True
      let position = getPosition "n5k1/1P3Pn1/1n5p/P1pP1R2/8/3q1B2/4Nr1P/R3K2R w Q - 0 1"
      isCheck position White `shouldBe` False
      isCheck position Black `shouldBe` True
      let position = getPosition "n5k1/1P2P1n1/1n5p/P1pP1R2/8/3q1B2/4N2P/R3Kr1R w Q - 0 1"
      isCheck position White `shouldBe` True
      isCheck position Black `shouldBe` False

  describe "moveFromAlgebraicMove" $ do
    it "Makes a move from a position and returns a new position" $ do
      algebraicMoveFromMove (moveFromAlgebraicMove "a1h8") `shouldBe` "a1h8"
      algebraicMoveFromMove (moveFromAlgebraicMove "h1a8") `shouldBe` "h1a8"
      algebraicMoveFromMove (moveFromAlgebraicMove "h7g8b") `shouldBe` "h7g8b"

  describe "fromSquarePart" $ do
    it "Gets the Square for the from part of a compact move" $ do
      fromSquarePart (moveFromAlgebraicMove "h1a8") `shouldBe` bitRefFromAlgebraicSquareRef "h1"

  describe "toSquarePart" $ do
    it "Gets the Square for the to part of a compact move" $ do
      toSquarePart (moveFromAlgebraicMove "h1a8") `shouldBe` bitRefFromAlgebraicSquareRef "a8"

  describe "movePieceWithinBitboard" $ do
    it "Returns a bitboard with the one bit in 'from', if it exists, moved to 'to'" $ do
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "a8") (bitRefFromAlgebraicSquareRef "b8") 0b1000100000000000000000000000000000001000010000000000000000000000
        `shouldBe` 0b0100100000000000000000000000000000001000010000000000000000000000
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "h1") (bitRefFromAlgebraicSquareRef "b8") 0b1000100000000000000000000000000000001000010000000000000000000001
        `shouldBe` 0b1100100000000000000000000000000000001000010000000000000000000000
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "a1") (bitRefFromAlgebraicSquareRef "b8") 0b1000100000000000000000000000000000001000010000000000000010000001
        `shouldBe` 0b1100100000000000000000000000000000001000010000000000000000000001
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "a8") (bitRefFromAlgebraicSquareRef "b8") 0b1000100000000000000000000000000000001000010000000000000000000000
        `shouldBe` 0b0100100000000000000000000000000000001000010000000000000000000000
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "h1") (bitRefFromAlgebraicSquareRef "a8") 0b0000100000000000000000000000000000001000010000000000000000000001
        `shouldBe` 0b1000100000000000000000000000000000001000010000000000000000000000
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "a1") (bitRefFromAlgebraicSquareRef "a8") 0b0000100000000000000000000000000000001000010000000000000010000001
        `shouldBe` 0b1000100000000000000000000000000000001000010000000000000000000001
      movePieceWithinBitboard (bitRefFromAlgebraicSquareRef "b8") (bitRefFromAlgebraicSquareRef "c8") 0b0000100000000000000000000000000000001000010000000000000010000001
        `shouldBe` 0b0000100000000000000000000000000000001000010000000000000010000001

  describe "removePieceFromBitboard" $ do
    it "Returns a bitboard with the one bit slipped to zero for the given square" $ do
      removePieceFromBitboard (bitRefFromAlgebraicSquareRef "a8") 0b1000100000000000000000000000000000001000010000000000000000000000
        `shouldBe` 0b0000100000000000000000000000000000001000010000000000000000000000
      removePieceFromBitboard (bitRefFromAlgebraicSquareRef "b8") 0b1100100000000000000000000000000000001000010000000000000000000000
        `shouldBe` 0b1000100000000000000000000000000000001000010000000000000000000000

  describe "enPassantCapturedPieceSquare" $ do
    it "Returns the square the captured pawn was on before it was captured en passant" $ do
      enPassantCapturedPieceSquare (bitRefFromAlgebraicSquareRef "a3") `shouldBe` (bitRefFromAlgebraicSquareRef "a4")
      enPassantCapturedPieceSquare (bitRefFromAlgebraicSquareRef "a6") `shouldBe` (bitRefFromAlgebraicSquareRef "a5")

  describe "removePawnIfPromotion" $ do
    it "Removes the bit from the pawn bitboard if it has just moved to a promotion rank" $ do
      removePawnIfPromotion 0b1000000000000000000000000000000000000000000000000000000000000000 `shouldBe` 0b0000000000000000000000000000000000000000000000000000000000000000

  describe "isPromotionSquare" $ do
    it "Returns True if the given square is on the first or eigth ranks" $ do
      isPromotionSquare (bitRefFromAlgebraicSquareRef "a8") `shouldBe` True
      isPromotionSquare (bitRefFromAlgebraicSquareRef "b1") `shouldBe` True
      isPromotionSquare (bitRefFromAlgebraicSquareRef "a3") `shouldBe` False

  describe "promotionPieceFromMove" $ do
    it "Returns the promotion piece from the move" $ do
      promotionPieceFromMove (moveFromAlgebraicMove "g7h8r") `shouldBe` Rook
      promotionPieceFromMove (moveFromAlgebraicMove "g7h8q") `shouldBe` Queen
      promotionPieceFromMove (moveFromAlgebraicMove "g7h8n") `shouldBe` Knight
      promotionPieceFromMove (moveFromAlgebraicMove "g7h8b") `shouldBe` Bishop
      promotionPieceFromMove (moveFromAlgebraicMove "g6h7") `shouldBe` Pawn

  describe "createIfPromotion" $ do
    it "Adds the promotion piece location to the bitboard" $ do
      createIfPromotion True 0b0000000010000000000000000000000000000000000000000000000000000000 0b0000000000000000000000000000000000000000000000000000000000000000 (bitRefFromAlgebraicSquareRef "a7") (bitRefFromAlgebraicSquareRef "a8")
        `shouldBe` 0b1000000000000000000000000000000000000000000000000000000000000000
      createIfPromotion True 0b0000000010000000000000000000000000000000000000000000000000000000 0b0000000000000000000000000000000000000000000000000000000000000000 (bitRefFromAlgebraicSquareRef "a7") (bitRefFromAlgebraicSquareRef "a6")
        `shouldBe` 0b0000000000000000000000000000000000000000000000000000000000000000
      createIfPromotion False 0b0000000010000000000000000000000000000000000000000000000000000000 0b0000000000000000000000000000000000000000000000000000000000000000 (bitRefFromAlgebraicSquareRef "a7") (bitRefFromAlgebraicSquareRef "a8")
        `shouldBe` 0b0000000000000000000000000000000000000000000000000000000000000000

  describe "makeMove" $ do
    it "Makes a move from a position and returns a new position" $ do
      makeMove (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
               (moveFromAlgebraicMove "e2e3")
                  `shouldBe` (getPosition "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
      makeMove (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
               (moveFromAlgebraicMove "e2e7")
                  `shouldBe` (getPosition "rnbqkbnr/ppppPppp/8/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
      makeMove (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1")
               (moveFromAlgebraicMove "e1g1")
                  `shouldBe` (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQ1RK1 b kq - 1 1")
      makeMove (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1")
               (moveFromAlgebraicMove "h1g1")
                  `shouldBe` (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK1R1 b kqQ - 1 1")
      makeMove (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQK2R w KQkq - 0 1")
               (moveFromAlgebraicMove "e2e3")
                  `shouldBe` (getPosition "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQK2R b KQkq - 0 1")
      makeMove (getPosition "r3k2r/pppppppp/2n1b3/2bn1q2/8/4P3/PPPP1PPP/RNBQK2R b KQq - 0 1")
               (moveFromAlgebraicMove "e8c8")
                  `shouldBe` (getPosition "2kr3r/pppppppp/2n1b3/2bn1q2/8/4P3/PPPP1PPP/RNBQK2R w KQ - 1 2")
      makeMove (getPosition "r3k2r/pppppppp/2n1b3/2bn1q2/8/4P3/PPPP1PPP/RNBQK2R b KQq - 0 1")
               (moveFromAlgebraicMove "e8d8")
                  `shouldBe` (getPosition "r2k3r/pppppppp/2n1b3/2bn1q2/8/4P3/PPPP1PPP/RNBQK2R w KQ - 1 2")
      makeMove (getPosition "r3k2r/pppppppp/2n1b3/2bn1q2/8/4P3/PPPP1PPP/RNBQK2R b KQq - 0 1")
               (moveFromAlgebraicMove "h8g8")
                  `shouldBe` (getPosition "r3k1r1/pppppppp/2n1b3/2bn1q2/8/4P3/PPPP1PPP/RNBQK2R w KQq - 1 2")
      makeMove (getPosition "2kr3r/pppppp1p/2n1b3/2bn1q2/4Pp2/8/PPPP1PPP/RNBQK2R b KQ e3 15 1")
               (moveFromAlgebraicMove "f4e3")
                  `shouldBe` (getPosition "2kr3r/pppppp1p/2n1b3/2bn1q2/8/4p3/PPPP1PPP/RNBQK2R w KQ - 0 2")
      makeMove (getPosition "2kr3r/ppppppPp/2n1b3/2bn1q2/8/4p3/PPPP1P1P/RNBQK2R w KQ - 12 1")
               (moveFromAlgebraicMove "g7h8r")
                  `shouldBe` (getPosition "2kr3R/pppppp1p/2n1b3/2bn1q2/8/4p3/PPPP1P1P/RNBQK2R b KQ - 0 1")
      makeMove (getPosition "2kr3R/pppp1p1p/2n1b3/2bn1q2/8/4p3/PPPP1PpP/RNBQK2R b KQ - 0 1")
               (moveFromAlgebraicMove "g2g1q")
                  `shouldBe` (getPosition "2kr3R/pppp1p1p/2n1b3/2bn1q2/8/4p3/PPPP1P1P/RNBQK1qR w KQ - 0 2")
      makeMove (getPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
               (moveFromAlgebraicMove "e2e4")
                  `shouldBe` (getPosition "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1")

      