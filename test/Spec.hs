import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Bitboards

main :: IO ()
main = hspec $ do
  describe "southFill" $ do
    it "South fills a bitboard correctly" $ do
      southFill 4611936708517363882 `shouldBe` (4629952088967215103 :: Int)

--    it "returns the first element of an *arbitrary* list" $
--      property $ \x xs -> head (x:xs) == (x :: Int)
--
--    it "throws an exception if used with an empty list" $ do
--      evaluate (head []) `shouldThrow` anyException