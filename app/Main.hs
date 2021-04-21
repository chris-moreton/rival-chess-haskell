module Main where

import Search.Perft
import Types
import Util.Fen
import Data.Time
import Data.Time.Clock.POSIX

main :: IO ()
main = do
  start <- round `fmap` getPOSIXTime
  print (perft (getPosition "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -") 5)
  end <- round `fmap` getPOSIXTime
  print (end - start)
