module Search.MoveConstants where

import Alias ( Move )

promotionQueenMoveMask :: Move
promotionQueenMoveMask = 192

promotionRookMoveMask :: Move
promotionRookMoveMask = 64

promotionBishopMoveMask :: Move
promotionBishopMoveMask = 128

promotionKnightMoveMask :: Move
promotionKnightMoveMask = 256

promotionFullMoveMask :: Move
promotionFullMoveMask = 448

enPassantNotAvailable :: Int
enPassantNotAvailable = -1