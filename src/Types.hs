module Types where

type Square = Int
type Bitboard = Int
type Move = Int

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
}

data CastlePrivileges = CastlePrivileges {
    whiteKingCastleAvailable  :: Bool
  , blackKingCastleAvailable  :: Bool
  , whiteQueenCastleAvailable :: Bool
  , blackQueenCastleAvailable :: Bool
}

data Position = Position {
    positionBitboards :: PieceBitboards
  , mover :: Mover
  , enPassantSquare :: Square
  , positionCastlePrivs :: CastlePrivileges
  , halfMoves :: Int
  , moveNumber :: Int
}

data Game = Game {
    gamePosition :: Position
  , gameMoves :: [Move]
}