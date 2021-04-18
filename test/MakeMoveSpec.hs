module MakeMoveSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Util.Fen

main :: IO ()
main = hspec $ do
--  describe "makeMove" $ do
--    it "Makes a move from a position and returns a new position" $ do
--      let position = getPosition "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 w kQKq g3 5 56"

  describe "makeMove" $ do
    it "Makes a move from a position and returns a new position" $ do
      algebraicMoveFromMove (moveFromAlgebraicMove "a1h8") `shouldBe` "a1h8"