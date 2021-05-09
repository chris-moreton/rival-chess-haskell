{-# LANGUAGE OverloadedStrings #-}

module Util.Utils where

import Data.List
import Types
import Alias
import Data.Bits
import Search.MoveConstants
import GHC.Compact
import Control.Parallel

substring :: String -> Int -> Int -> String
substring text start end = take (end - start) (drop start text)

fromSquareMask :: Square -> Move
fromSquareMask 0 = 0
fromSquareMask 1 = 65536
fromSquareMask 2 = 131072
fromSquareMask 3 = 196608
fromSquareMask 4 = 262144
fromSquareMask 5 = 327680
fromSquareMask 6 = 393216
fromSquareMask 7 = 458752
fromSquareMask 8 = 524288
fromSquareMask 9 = 589824
fromSquareMask 10 = 655360
fromSquareMask 11 = 720896
fromSquareMask 12 = 786432
fromSquareMask 13 = 851968
fromSquareMask 14 = 917504
fromSquareMask 15 = 983040
fromSquareMask 16 = 1048576
fromSquareMask 17 = 1114112
fromSquareMask 18 = 1179648
fromSquareMask 19 = 1245184
fromSquareMask 20 = 1310720
fromSquareMask 21 = 1376256
fromSquareMask 22 = 1441792
fromSquareMask 23 = 1507328
fromSquareMask 24 = 1572864
fromSquareMask 25 = 1638400
fromSquareMask 26 = 1703936
fromSquareMask 27 = 1769472
fromSquareMask 28 = 1835008
fromSquareMask 29 = 1900544
fromSquareMask 30 = 1966080
fromSquareMask 31 = 2031616
fromSquareMask 32 = 2097152
fromSquareMask 33 = 2162688
fromSquareMask 34 = 2228224
fromSquareMask 35 = 2293760
fromSquareMask 36 = 2359296
fromSquareMask 37 = 2424832
fromSquareMask 38 = 2490368
fromSquareMask 39 = 2555904
fromSquareMask 40 = 2621440
fromSquareMask 41 = 2686976
fromSquareMask 42 = 2752512
fromSquareMask 43 = 2818048
fromSquareMask 44 = 2883584
fromSquareMask 45 = 2949120
fromSquareMask 46 = 3014656
fromSquareMask 47 = 3080192
fromSquareMask 48 = 3145728
fromSquareMask 49 = 3211264
fromSquareMask 50 = 3276800
fromSquareMask 51 = 3342336
fromSquareMask 52 = 3407872
fromSquareMask 53 = 3473408
fromSquareMask 54 = 3538944
fromSquareMask 55 = 3604480
fromSquareMask 56 = 3670016
fromSquareMask 57 = 3735552
fromSquareMask 58 = 3801088
fromSquareMask 59 = 3866624
fromSquareMask 60 = 3932160
fromSquareMask 61 = 3997696
fromSquareMask 62 = 4063232
fromSquareMask 63 = 4128768

fromSquarePart :: Move -> Square
fromSquarePart move = move `shiftR` 16

toSquarePart :: Move -> Square
toSquarePart move = (.&.) move 63

promotionPieceFromMove :: Move -> Piece
promotionPieceFromMove move
    | promotionPart == 0 = Pawn
    | promotionPart == promotionQueenMoveMask = Queen
    | promotionPart == promotionRookMoveMask = Rook
    | promotionPart == promotionBishopMoveMask = Bishop
    | promotionPart == promotionKnightMoveMask = Knight
    | otherwise = Pawn
    where promotionPart = promotionFullMoveMask .&. move

opponent :: Position -> Mover
opponent position = if mover position == White then Black else White

switchSide :: Mover -> Mover
switchSide White = Black
switchSide Black = White
