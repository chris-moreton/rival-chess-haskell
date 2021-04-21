module Search.MoveConstants where

import Types (MoveMask)

promotionQueenMoveMask :: MoveMask
promotionQueenMoveMask = 192

promotionRookMoveMask :: MoveMask
promotionRookMoveMask = 64

promotionBishopMoveMask :: MoveMask
promotionBishopMoveMask = 128

promotionKnightMoveMask :: MoveMask
promotionKnightMoveMask = 256

promotionFullMoveMask :: MoveMask
promotionFullMoveMask = 448

enPassantNotAvailable :: Int
enPassantNotAvailable = -1