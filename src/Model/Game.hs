module Model.Game where

import Types

bitboardWhitePawnIndex = 0 :: Int
bitboardWhiteKnightIndex = 1 :: Int
bitboardWhiteBishopIndex = 2 :: Int
bitboardWhiteQueenIndex = 3 :: Int
bitboardWhiteKingIndex = 4 :: Int
bitboardWhiteRookIndex = 5 :: Int
bitboardBlackPawnIndex = 6 :: Int
bitboardBlackKnightIndex = 7 :: Int
bitboardBlackBishopIndex = 8 :: Int
bitboardBlackQueenIndex = 9 :: Int
bitboardBlackKingIndex = 10 :: Int
bitboardBlackRookIndex = 11 :: Int
bitboardAllIndex = 12 :: Int
bitboardFriendlyIndex = 13 :: Int
bitboardEnemyIndex = 14 :: Int
bitboardEnPassantSquareIndex = 15 :: Int
bitboardNoneIndex = 16 :: Int
bitboardCountIndex = 17 :: Int

whiteBitboardTypes = [bitboardWhitePawnIndex,bitboardWhiteKnightIndex,bitboardWhiteBishopIndex,bitboardWhiteRookIndex,bitboardWhiteQueenIndex,bitboardWhiteKingIndex]
blackBitboardTypes = [bitboardBlackPawnIndex,bitboardBlackKnightIndex,bitboardBlackBishopIndex,bitboardBlackRookIndex,bitboardBlackQueenIndex,bitboardBlackKingIndex]
allBitboardTypes = whiteBitboardTypes ++ blackBitboardTypes

