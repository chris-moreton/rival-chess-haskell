{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Search.MoveUtils where

import Types
import Alias
import Util.Fen
import Util.Utils
import Data.Bits
import Util.Bitboards
import Search.MoveConstants

{-# INLINE removePieceFromBitboard #-}
removePieceFromBitboard :: Square -> Bitboard -> Bitboard
removePieceFromBitboard !square = (.&.) (complement (bit square))

{-# INLINE moveWhiteRookWhenCastling #-}
moveWhiteRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveWhiteRookWhenCastling !from !to !kingBoard !rookBoard
  | from == e1Bit && to == g1Bit && testBit kingBoard e1Bit = movePieceWithinBitboard h1Bit f1Bit rookBoard
  | from == e1Bit && to == c1Bit && testBit kingBoard e1Bit = movePieceWithinBitboard a1Bit d1Bit rookBoard
  | otherwise = rookBoard

{-# INLINE moveBlackRookWhenCastling #-}
moveBlackRookWhenCastling :: Square -> Square -> Bitboard -> Bitboard -> Bitboard
moveBlackRookWhenCastling !from !to !kingBoard !rookBoard
  | from == e8Bit && to == g8Bit && testBit kingBoard e8Bit = movePieceWithinBitboard h8Bit f8Bit rookBoard
  | from == e8Bit && to == c8Bit && testBit kingBoard e8Bit = movePieceWithinBitboard a8Bit d8Bit rookBoard
  | otherwise = rookBoard

{-# INLINE enPassantCapturedPieceSquare #-}
enPassantCapturedPieceSquare :: Square -> Square
enPassantCapturedPieceSquare 16 = 24
enPassantCapturedPieceSquare 17 = 25
enPassantCapturedPieceSquare 18 = 26
enPassantCapturedPieceSquare 19 = 27
enPassantCapturedPieceSquare 20 = 28
enPassantCapturedPieceSquare 21 = 29
enPassantCapturedPieceSquare 22 = 30
enPassantCapturedPieceSquare 23 = 31
enPassantCapturedPieceSquare 40 = 32
enPassantCapturedPieceSquare 41 = 33
enPassantCapturedPieceSquare 42 = 34
enPassantCapturedPieceSquare 43 = 35
enPassantCapturedPieceSquare 44 = 36
enPassantCapturedPieceSquare 45 = 37
enPassantCapturedPieceSquare 46 = 38
enPassantCapturedPieceSquare 47 = 39

removePawnWhenEnPassant :: Bitboard -> Bitboard -> Square -> Square -> Bitboard
{-# INLINE removePawnWhenEnPassant #-}
removePawnWhenEnPassant !attackerBb !defenderBb !to !enPassantSquare
  | enPassantSquare == to && testBit attackerBb to = removePieceFromBitboard (enPassantCapturedPieceSquare to) defenderBb
  | otherwise = defenderBb

removePawnIfPromotion :: Bitboard -> Bitboard
{-# INLINE removePawnIfPromotion #-}
removePawnIfPromotion !bb = bb .&. 0b0000000011111111111111111111111111111111111111111111111100000000

isPromotionSquare :: Square -> Bool
{-# INLINE isPromotionSquare #-}
isPromotionSquare !sq = testBit promotionSquares sq

createIfPromotion :: Bool -> Bitboard -> Bitboard -> Square -> Square -> Bitboard
{-# INLINE createIfPromotion #-}
createIfPromotion !isPromotionPiece !pawnBitboard !pieceBitboard !fromSquare !toSquare
  | isPromotionPiece && isPromotionSquare toSquare && testBit pawnBitboard fromSquare = pieceBitboard .|. bit toSquare
  | otherwise = pieceBitboard

movePieceWithinBitboard :: Square -> Square -> Bitboard -> Bitboard
{-# INLINE movePieceWithinBitboard #-}
movePieceWithinBitboard !from !to !bb
  | testBit bb from = (.|.) (clearBit bb from) (bit to)
  | otherwise = clearBit bb to
