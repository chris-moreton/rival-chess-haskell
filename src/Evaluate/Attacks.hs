module Evaluate.Attacks where

import Types ( Position (..), Mover (..) )
import Alias ( Bitboard )

data Attacks = Attacks {
     whitePawnsAttackBitboard :: Bitboard 
   , blackPawnsAttackBitboard :: Bitboard 
   , allBitboard              :: Bitboard 
   , whiteRooksAttackList     :: [Bitboard]
   , whiteBishopsAttackList   :: [Bitboard]
   , whiteQueensAttackList    :: [Bitboard]
   , blackRooksAttackList     :: [Bitboard]
   , blackBishopsAttackList   :: [Bitboard]
   , blackQueensAttackList    :: [Bitboard]
}

allAttacks :: Position -> Attacks
allAttacks position = Attacks {
      whitePawnsAttackBitboard = whitePawnAttacks (whitePawnBitboard position)
    , blackPawnsAttackBitboard = blackPawnAttacks (blackPawnBitboard position)
    , whiteRooksAttackList     = attackList abb (whiteRookBitboard position)   rookAttacks White
    , whiteBishopsAttackList   = attackList abb (whiteBishopBitboard position) bishopAttacks White
    , whiteQueensAttackList    = attackList abb (whiteQueenBitboard position)  queenAttacks White
    , blackRooksAttackList     = attackList abb (blackRookBitboard position)   rookAttacks White
    , blackBishopsAttackList   = attackList abb (blackBishopBitboard position) bishopAttacks White
    , blackQueensAttackList    = attackList abb (blackQueenBitboard position)  queenAttacks White
    , allBitboard = abb
  }
  where
    abb = allPiecesBitboard position

attackList :: Bitboard -> Bitboard -> (Bitboard -> Int -> Bitboard) -> Mover -> [Bitboard]
attackList allBitboard squaresBitboard pieceAttacksFn colour = []

whitePawnAttacks :: Bitboard -> Bitboard
whitePawnAttacks bb = 0

blackPawnAttacks :: Bitboard -> Bitboard
blackPawnAttacks bb = 0

rookAttacks :: Bitboard -> Int -> Bitboard
rookAttacks allBitboard square = 0

bishopAttacks :: Bitboard -> Int -> Bitboard
bishopAttacks allBitboard square = 0

queenAttacks :: Bitboard -> Int -> Bitboard
queenAttacks allBitboard square = 0

