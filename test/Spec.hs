import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Util.Bitboards
import Util.Fen

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
      fenRanks "6k1/6p1/1p2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8" `shouldBe` ["6k1","6p1","1p2q2p","1p5P","1P3RP1","2PK1B2","1r2N3","8"]

  describe "rankBits" $ do
    it "Converts from FEN rank string into char array of eight 0s and 1s for a given piece" $ do
      rankBits "8" 'Q' `shouldBe` [0,0,0,0,0,0,0,0]
      rankBits "6k1" 'k' `shouldBe` [0,0,0,0,0,0,1,0]
      rankBits "6k1" 'q' `shouldBe` [0,0,0,0,0,0,0,0]
      rankBits "6p1" 'p' `shouldBe` [0,0,0,0,0,0,1,0]
      rankBits "6pp" 'p' `shouldBe` [0,0,0,0,0,0,1,1]
      rankBits "P7" 'P' `shouldBe` [1,0,0,0,0,0,0,0]
      rankBits "1p2q2p" 'p' `shouldBe` [0,1,0,0,0,0,0,1]

--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
--
--    it "throws an exception if used with an empty list" $ do
--      evaluate (head []) `shouldThrow` anyException