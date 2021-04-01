import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Util.Bitboards
import Util.Fen
import Model.Game

main :: IO ()
main = hspec $ do
  describe "southFill" $ do
    it "South fills a bitboard" $ do
      southFill 4611936708517363882 `shouldBe` (4629952088967215103 :: Int)

  describe "northFill" $ do
    it "North fills a bitboard" $ do
      northFill 4611936708517363882 `shouldBe` (-1332566 :: Int)

  describe "rankBitboards" $ do
    it "Calculates rank8Bitboards" $ do
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

  describe "algebraicMoveFromCompactMove" $ do
    it "Converts a compact move to an algebraic move" $ do
      algebraicMoveFromCompactMove 458808 `shouldBe` "a1h8"

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
