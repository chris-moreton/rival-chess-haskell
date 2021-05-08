{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Array.IArray
import Data.Array.Unboxed
import GHC.Generics
import GHC.Compact
import Alias

data Mover = White | Black deriving (Enum,Show,Eq)
data Piece = Pawn | King | Queen | Bishop | Knight | Rook deriving (Enum,Show,Eq)

data Position = Position {
    whitePawnBitboard :: {-# UNPACK #-} !Bitboard
  , whiteKnightBitboard :: {-# UNPACK #-} !Bitboard
  , whiteBishopBitboard :: {-# UNPACK #-} !Bitboard
  , whiteQueenBitboard :: {-# UNPACK #-} !Bitboard
  , whiteKingBitboard :: {-# UNPACK #-} !Bitboard
  , whiteRookBitboard :: {-# UNPACK #-} !Bitboard
  , blackPawnBitboard :: {-# UNPACK #-} !Bitboard
  , blackKnightBitboard :: {-# UNPACK #-} !Bitboard
  , blackBishopBitboard :: {-# UNPACK #-} !Bitboard
  , blackQueenBitboard :: {-# UNPACK #-} !Bitboard
  , blackKingBitboard :: {-# UNPACK #-} !Bitboard
  , blackRookBitboard :: {-# UNPACK #-} !Bitboard
  , allPiecesBitboard :: {-# UNPACK #-} !Bitboard
  , whitePiecesBitboard :: {-# UNPACK #-} !Bitboard
  , blackPiecesBitboard :: {-# UNPACK #-} !Bitboard
  , mover :: !Mover
  , enPassantSquare :: {-# UNPACK #-} !Square
  , whiteKingCastleAvailable  :: !Bool
  , blackKingCastleAvailable  :: !Bool
  , whiteQueenCastleAvailable :: !Bool
  , blackQueenCastleAvailable :: !Bool
  , halfMoves :: Int
  , moveNumber :: Int
} deriving (Generic,Show,Eq)
