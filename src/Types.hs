{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Array.IArray
import qualified Data.DList as DList
import qualified Data.Vector.Storable as V
import GHC.Generics
import GHC.Compact

type Square = Int
type Bitboard = Word
type Move = Int
type BitboardArray = V.Vector Bitboard
type MoveList = DList.DList Move
type MagicMoves = Array Int (V.Vector Bitboard)

data Mover = White | Black deriving (Enum,Show,Eq)
data Piece = Pawn | King | Queen | Bishop | Knight | Rook deriving (Enum,Show,Eq)

data Position = Position {
    whitePawnBitboard :: Bitboard
  , whiteKnightBitboard :: Bitboard
  , whiteBishopBitboard :: Bitboard
  , whiteQueenBitboard :: Bitboard
  , whiteKingBitboard :: Bitboard
  , whiteRookBitboard :: Bitboard
  , blackPawnBitboard :: Bitboard
  , blackKnightBitboard :: Bitboard
  , blackBishopBitboard :: Bitboard
  , blackQueenBitboard :: Bitboard
  , blackKingBitboard :: Bitboard
  , blackRookBitboard :: Bitboard
  , allPiecesBitboard :: Bitboard
  , mover :: Mover
  , enPassantSquare :: Square
  , whiteKingCastleAvailable  :: Bool
  , blackKingCastleAvailable  :: Bool
  , whiteQueenCastleAvailable :: Bool
  , blackQueenCastleAvailable :: Bool
  , halfMoves :: Int
  , moveNumber :: Int
} deriving (Generic,Show,Eq)
