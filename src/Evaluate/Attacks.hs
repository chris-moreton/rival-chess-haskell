module Evaluate.Attacks where

import Types ( Position )
import Alias ( Bitboard )

data Attacks = Attacks {
     whitePawnsAttackBitboard :: Bitboard 
   , blackPawnsAttackBitboard :: Bitboard 
   , allBitboard              :: Bitboard 
   , whiteRooksAttackList     :: [Bitboard]
   , whiteBishopsAttackArray  :: [Bitboard]
   , whiteQueensAttackArray   :: [Bitboard]
   , blackRooksAttackArray    :: [Bitboard]
   , blackBishopsAttackArray  :: [Bitboard]
   , blackQueensAttackArray   :: [Bitboard]
}

allAttacks :: Position -> Attacks
allAttacks position = Attacks 0 0 0 [] [] [] [] [] []