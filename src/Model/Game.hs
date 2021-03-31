module Model.Game where

type Square = Int
type Bitboard = Int
type Move = Int

data Mover = White | Black deriving (Enum)

data PieceBitboards = PieceBitboards {
    whitePawnBitboard :: Bitboard
  , whiteKnightBitboard :: Bitboard
  , whiteBishopBitboard :: Bitboard
  , whiteQueenBitboard :: Bitboard
  , whiteKing :: Bitboard
  , whiteRookBitboard :: Bitboard  
  , blackPawnBitboard :: Bitboard
  , blackKnightBitboard :: Bitboard
  , blackBishopBitboard :: Bitboard
  , blackQueenBitboard :: Bitboard
  , blackKing :: Bitboard
  , blackRookBitboard :: Bitboard  
}

data CastlePrivileges = CastlePrivileges {
    whiteKingCastleAvailable  :: Bool
  , blackKingCastleAvailable  :: Bool
  , whiteQueenCastleAvailable :: Bool
  , blackQueenCastleAvailable :: Bool
}

data Position = Position {
    bitboards :: PieceBitboards
  , mover :: Mover
  , enPassantSquare :: Square
  , castlePrivs :: CastlePrivileges
  , halfMoves :: Int
  , moveNumber :: Int
}

data Game = Game {
    position :: Position
  , moves :: [Move]
}