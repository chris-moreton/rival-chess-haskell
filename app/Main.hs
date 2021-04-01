module Main where

import Bitboards

main :: IO ()
main = do
  print $ map (\x -> algebraicMoveFromCompactMove (knightMoves (position "n5k1/6n1/1n2q2p/1p5P/1P3RP1/2PK1B2/1r2N3/8 b kQKq g3 5 56")))
