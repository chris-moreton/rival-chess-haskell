{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Array.IArray
import qualified Data.DList as DList
import qualified Data.Vector.Storable as V
import GHC.Generics

type Square = Int
type Bitboard = Word
type Move = Int
type BitboardArray = V.Vector Bitboard
type MoveList = DList.DList Move

data Mover = White | Black deriving (Enum,Show,Eq)
data Piece = Pawn | King | Queen | Bishop | Knight | Rook deriving (Enum,Show,Eq)

data PieceBitboards = PieceBitboards {
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
} deriving (Generic,Show,Eq)

data CastlePrivileges = CastlePrivileges {
    whiteKingCastleAvailable  :: Bool
  , blackKingCastleAvailable  :: Bool
  , whiteQueenCastleAvailable :: Bool
  , blackQueenCastleAvailable :: Bool
} deriving (Generic,Show,Eq)

data Position = Position {
    positionBitboards :: PieceBitboards
  , mover :: Mover
  , enPassantSquare :: Square
  , positionCastlePrivs :: CastlePrivileges
  , halfMoves :: Int
  , moveNumber :: Int
} deriving (Generic,Show,Eq)

data Game = Game {
    gamePosition :: Position
  , gameMoves :: [Move]
}