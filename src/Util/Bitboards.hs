{-# LANGUAGE BinaryLiterals,NegativeLiterals,StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Util.Bitboards where

import Data.Bits
import Util.Utils
import Types
import Data.Array.IArray
import GHC.Compact
import qualified Data.Vector.Storable as V
import Util.MagicMovesBishop

bitString :: Bitboard -> String
bitString bitboard = recurBitString bitboard 63 ""

recurBitString :: Bitboard -> Int -> String -> String
recurBitString _ (-1) result = result
recurBitString bitboard square result = do
  let bitMask = bit square
  recurBitString (xor bitboard bitMask) (square - 1) (result ++ if bitMask == (.&.) bitMask bitboard then "1" else "0")

enemyBitboard :: Position -> Bitboard
enemyBitboard !position 
    | mover position == White = blackPiecesBitboard position
    | otherwise = whitePiecesBitboard position

promotionSquares :: Bitboard
promotionSquares = 0b1111111100000000000000000000000000000000000000000000000011111111

emptySquaresBitboard :: Position -> Bitboard
emptySquaresBitboard !position = complement (allPiecesBitboard position)

orWithURightShiftedSelf :: Bitboard -> Int -> Bitboard
orWithURightShiftedSelf !x !y = (.|.) x (shiftR x y)

orWithULeftShiftedSelf :: Bitboard -> Int -> Bitboard
orWithULeftShiftedSelf !x !y = (.|.) x (shiftL x y)

southFill :: Bitboard -> Bitboard
southFill x = orWithURightShiftedSelf (orWithURightShiftedSelf (orWithURightShiftedSelf x 8) 16) 32

northFill :: Bitboard -> Bitboard
northFill x = orWithULeftShiftedSelf (orWithULeftShiftedSelf (orWithULeftShiftedSelf x 8) 16) 32

everyEighthBitFrom :: Int -> Bitboard
everyEighthBitFrom x = if x < 8 then shiftL 1 x else (.|.) (shiftL 1 x) (everyEighthBitFrom ((-) x 8))

setBits :: [Int] -> Bitboard
setBits [] = 0
setBits [x] = shiftL 1 x
setBits xs = (.|.) (shiftL 1 (head xs)) (setBits (tail xs))

all64BitsSet :: Word
all64BitsSet = 18446744073709551615

rank1Bits :: Bitboard
rank1Bits = setBits [0,1,2,3,4,5,6,7]
rank2Bits :: Bitboard
rank2Bits = shiftL rank1Bits 8
rank3Bits :: Bitboard
rank3Bits = shiftL rank2Bits 8
rank4Bits :: Bitboard
rank4Bits = shiftL rank3Bits 8
rank5Bits :: Bitboard
rank5Bits = shiftL rank4Bits 8
rank6Bits :: Bitboard
rank6Bits = shiftL rank5Bits 8
rank7Bits :: Bitboard
rank7Bits = shiftL rank6Bits 8
rank8Bits :: Bitboard
rank8Bits = shiftL rank7Bits 8

fileABits = everyEighthBitFrom a8Bit
fileBBits = everyEighthBitFrom b8Bit
fileCBits = everyEighthBitFrom c8Bit
fileDBits = everyEighthBitFrom d8Bit
fileEBits = everyEighthBitFrom e8Bit
fileFBits = everyEighthBitFrom f8Bit
fileGBits = everyEighthBitFrom g8Bit
fileHBits = everyEighthBitFrom h8Bit

f1G1Bits = setBits [f1Bit,g1Bit]
g1H1Bits = setBits [g1Bit,h1Bit]
a1B1Bits = setBits [a1Bit,b1Bit]
b1C1Bits = setBits [b1Bit,c1Bit]
f8G8Bits = setBits [f8Bit,g8Bit]
g8H8Bits = setBits [g8Bit,h8Bit]
a8B8Bits = setBits [a8Bit,b8Bit]
b8C8Bits = setBits [b8Bit,c8Bit]

a1Bit = 7 :: Int
b1Bit = 6 :: Int
c1Bit = 5 :: Int
d1Bit = 4 :: Int
e1Bit = 3 :: Int
f1Bit = 2 :: Int
g1Bit = 1 :: Int
h1Bit = 0 :: Int

a2Bit = (+) a1Bit 8
b2Bit = (+) b1Bit 8
c2Bit = (+) c1Bit 8
d2Bit = (+) d1Bit 8
e2Bit = (+) e1Bit 8
f2Bit = (+) f1Bit 8
g2Bit = (+) g1Bit 8
h2Bit = (+) h1Bit 8

a3Bit = (+) a2Bit 8
b3Bit = (+) b2Bit 8
c3Bit = (+) c2Bit 8
d3Bit = (+) d2Bit 8
e3Bit = (+) e2Bit 8
f3Bit = (+) f2Bit 8
g3Bit = (+) g2Bit 8
h3Bit = (+) h2Bit 8

a4Bit = (+) a3Bit 8
b4Bit = (+) b3Bit 8
c4Bit = (+) c3Bit 8
d4Bit = (+) d3Bit 8
e4Bit = (+) e3Bit 8
f4Bit = (+) f3Bit 8
g4Bit = (+) g3Bit 8
h4Bit = (+) h3Bit 8

a5Bit = (+) a4Bit 8
b5Bit = (+) b4Bit 8
c5Bit = (+) c4Bit 8
d5Bit = (+) d4Bit 8
e5Bit = (+) e4Bit 8
f5Bit = (+) f4Bit 8
g5Bit = (+) g4Bit 8
h5Bit = (+) h4Bit 8

a6Bit = (+) a5Bit 8
b6Bit = (+) b5Bit 8
c6Bit = (+) c5Bit 8
d6Bit = (+) d5Bit 8
e6Bit = (+) e5Bit 8
f6Bit = (+) f5Bit 8
g6Bit = (+) g5Bit 8
h6Bit = (+) h5Bit 8

a7Bit = (+) a6Bit 8
b7Bit = (+) b6Bit 8
c7Bit = (+) c6Bit 8
d7Bit = (+) d6Bit 8
e7Bit = (+) e6Bit 8
f7Bit = (+) f6Bit 8
g7Bit = (+) g6Bit 8
h7Bit = (+) h6Bit 8

a8Bit = (+) a7Bit 8
b8Bit = (+) b7Bit 8
c8Bit = (+) c7Bit 8
d8Bit = (+) d7Bit 8
e8Bit = (+) e7Bit 8
f8Bit = (+) f7Bit 8
g8Bit = (+) g7Bit 8
h8Bit = (+) h7Bit 8

middleFiles8Bit = setBits [d1Bit,e1Bit]
nonMidFiles8Bit = setBits [a1Bit,b1Bit,c1Bit,f1Bit,g1Bit,h1Bit]

low32Bits = (.|.) rank1Bits ((.|.) rank2Bits ((.|.) rank3Bits rank4Bits))

darkSquaresBits :: Bitboard
darkSquaresBits = setBits [a1Bit,a3Bit,a5Bit,a7Bit,b2Bit,b4Bit,b6Bit,b8Bit,c1Bit,c3Bit,c5Bit,c7Bit,d2Bit,d4Bit,d6Bit,d8Bit,e1Bit,e3Bit,e5Bit,e7Bit,f2Bit,f4Bit,f6Bit,f8Bit,g1Bit,g3Bit,g5Bit,g7Bit,h2Bit,h4Bit,h6Bit,h8Bit] :: Bitboard
lightSquaresBits :: Bitboard
lightSquaresBits = setBits [a2Bit,a4Bit,a6Bit,a8Bit,b1Bit,b3Bit,b5Bit,b7Bit,c2Bit,c4Bit,c6Bit,c8Bit,d1Bit,d3Bit,d5Bit,d7Bit,e2Bit,e4Bit,e6Bit,e8Bit,f1Bit,f3Bit,f5Bit,f7Bit,g2Bit,g4Bit,g6Bit,g8Bit,h1Bit,h3Bit,h5Bit,h7Bit] :: Bitboard

knightMovesBitboards = V.fromList [0x20400,
         0x50800, 0xa1100, 0x142200, 0x284400, 0x508800, 0xa01000, 0x402000, 0x2040004,
         0x5080008, 0xa110011, 0x14220022, 0x28440044, 0x50880088, 0xa0100010, 0x40200020, 0x204000402,
         0x508000805, 0xa1100110a, 0x1422002214, 0x2844004428, 0x5088008850, 0xa0100010a0, 0x4020002040, 0x20400040200,
         0x50800080500, 0xa1100110a00, 0x142200221400, 0x284400442800, 0x508800885000, 0xa0100010a000, 0x402000204000, 0x2040004020000,
         0x5080008050000, 0xa1100110a0000, 0x14220022140000, 0x28440044280000, 0x50880088500000, 0xa0100010a00000, 0x40200020400000, 0x204000402000000,
         0x508000805000000, 0xa1100110a000000, 0x1422002214000000, 0x2844004428000000, 0x5088008850000000, -0x5fefffef60000000, 0x4020002040000000, 0x400040200000000,
         0x800080500000000, 0x1100110a00000000, 0x2200221400000000, 0x4400442800000000, -0x77ff77b000000000, 0x100010a000000000, 0x2000204000000000, 0x4020000000000,
         0x8050000000000, 0x110a0000000000, 0x22140000000000, 0x44280000000000, 0x88500000000000, 0x10a00000000000, 0x20400000000000] :: V.Vector Bitboard

kingMovesBitboards = V.fromList [
        0x302,
        0x705, 0xe0a, 0x1c14, 0x3828, 0x7050, 0xe0a0, 0xc040, 0x30203,
        0x70507, 0xe0a0e, 0x1c141c, 0x382838, 0x705070, 0xe0a0e0, 0xc040c0, 0x3020300,
        0x7050700, 0xe0a0e00, 0x1c141c00, 0x38283800, 0x70507000, 0xe0a0e000, 0xc040c000, 0x302030000,
        0x705070000, 0xe0a0e0000, 0x1c141c0000, 0x3828380000, 0x7050700000, 0xe0a0e00000, 0xc040c00000, 0x30203000000,
        0x70507000000, 0xe0a0e000000, 0x1c141c000000, 0x382838000000, 0x705070000000, 0xe0a0e0000000, 0xc040c0000000, 0x3020300000000,
        0x7050700000000, 0xe0a0e00000000, 0x1c141c00000000, 0x38283800000000, 0x70507000000000, 0xe0a0e000000000, 0xc040c000000000, 0x302030000000000,
        0x705070000000000, 0xe0a0e0000000000, 0x1c141c0000000000, 0x3828380000000000, 0x7050700000000000, -0x1f5f200000000000, -0x3fbf400000000000, 0x203000000000000,
        0x507000000000000, 0xa0e000000000000, 0x141c000000000000, 0x2838000000000000, 0x5070000000000000, -0x5f20000000000000, 0x40c0000000000000] :: V.Vector Bitboard

whitePawnMovesForward = V.fromList [
        0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000,
        0x10000, 0x20000, 0x40000, 0x80000, 0x100000, 0x200000, 0x400000, 0x800000,
        0x1000000, 0x2000000, 0x4000000, 0x8000000, 0x10000000, 0x20000000, 0x40000000, 0x80000000,
        0x100000000, 0x200000000, 0x400000000, 0x800000000, 0x1000000000, 0x2000000000, 0x4000000000, 0x8000000000,
        0x10000000000, 0x20000000000, 0x40000000000, 0x80000000000, 0x100000000000, 0x200000000000, 0x400000000000, 0x800000000000,
        0x1000000000000, 0x2000000000000, 0x4000000000000, 0x8000000000000, 0x10000000000000, 0x20000000000000, 0x40000000000000, 0x80000000000000,
        0x100000000000000, 0x200000000000000, 0x400000000000000, 0x800000000000000, 0x1000000000000000, 0x2000000000000000, 0x4000000000000000, 1 `shiftL` 63,
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0] :: V.Vector Bitboard
whitePawnMovesCapture = V.fromList [
        0x200, 0x500, 0xa00, 0x1400, 0x2800, 0x5000, 0xa000, 0x4000,
        0x20000, 0x50000, 0xa0000, 0x140000, 0x280000, 0x500000, 0xa00000, 0x400000,
        0x2000000, 0x5000000, 0xa000000, 0x14000000, 0x28000000, 0x50000000, 0xa0000000, 0x40000000,
        0x200000000, 0x500000000, 0xa00000000, 0x1400000000, 0x2800000000, 0x5000000000, 0xa000000000, 0x4000000000,
        0x20000000000, 0x50000000000, 0xa0000000000, 0x140000000000, 0x280000000000, 0x500000000000, 0xa00000000000, 0x400000000000,
        0x2000000000000, 0x5000000000000, 0xa000000000000, 0x14000000000000, 0x28000000000000, 0x50000000000000, 0xa0000000000000, 0x40000000000000,
        0x200000000000000, 0x500000000000000, 0xa00000000000000, 0x1400000000000000, 0x2800000000000000, 0x5000000000000000, -0x6000000000000000, 0x4000000000000000,
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0] :: V.Vector Bitboard

blackPawnMovesForward = V.fromList [
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        0x1, 0x2, 0x4, 0x8, 0x10, 0x20, 0x40, 0x80,
        0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000, 0x8000,
        0x10000, 0x20000, 0x40000, 0x80000, 0x100000, 0x200000, 0x400000, 0x800000,
        0x1000000, 0x2000000, 0x4000000, 0x8000000, 0x10000000, 0x20000000, 0x40000000, 0x80000000,
        0x100000000, 0x200000000, 0x400000000, 0x800000000, 0x1000000000, 0x2000000000, 0x4000000000, 0x8000000000,
        0x10000000000, 0x20000000000, 0x40000000000, 0x80000000000, 0x100000000000, 0x200000000000, 0x400000000000, 0x800000000000,
        0x1000000000000, 0x2000000000000, 0x4000000000000, 0x8000000000000, 0x10000000000000, 0x20000000000000, 0x40000000000000, 0x80000000000000] :: V.Vector Bitboard

blackPawnMovesCapture = V.fromList [
        0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
        0x2, 0x5, 0xa, 0x14, 0x28, 0x50, 0xa0, 0x40,
        0x200, 0x500, 0xa00, 0x1400, 0x2800, 0x5000, 0xa000, 0x4000,
        0x20000, 0x50000, 0xa0000, 0x140000, 0x280000, 0x500000, 0xa00000, 0x400000,
        0x2000000, 0x5000000, 0xa000000, 0x14000000, 0x28000000, 0x50000000, 0xa0000000, 0x40000000,
        0x200000000, 0x500000000, 0xa00000000, 0x1400000000, 0x2800000000, 0x5000000000, 0xa000000000, 0x4000000000,
        0x20000000000, 0x50000000000, 0xa0000000000, 0x140000000000, 0x280000000000, 0x500000000000, 0xa00000000000, 0x400000000000,
        0x2000000000000, 0x5000000000000, 0xa000000000000, 0x14000000000000, 0x28000000000000, 0x50000000000000, 0xa0000000000000, 0x40000000000000] :: V.Vector Bitboard

whitePassedPawnMask = V.fromList [
        0, 0, 0, 0, 0, 0, 0, 0,
        0x0003030303030000, 0x0007070707070000, 0x000E0E0E0E0E0000, 0x001C1C1C1C1C0000, 0x0038383838380000, 0x0070707070700000, 0x00E0E0E0E0E00000, 0x00C0C0C0C0C00000,
        0x0003030303000000, 0x0007070707000000, 0x000E0E0E0E000000, 0x001C1C1C1C000000, 0x0038383838000000, 0x0070707070000000, 0x00E0E0E0E0000000, 0x00C0C0C0C0000000,
        0x0003030300000000, 0x0007070700000000, 0x000E0E0E00000000, 0x001C1C1C00000000, 0x0038383800000000, 0x0070707000000000, 0x00E0E0E000000000, 0x00C0C0C000000000,
        0x0003030000000000, 0x0007070000000000, 0x000E0E0000000000, 0x001C1C0000000000, 0x0038380000000000, 0x0070700000000000, 0x00E0E00000000000, 0x00C0C00000000000,
        0x0003000000000000, 0x0007000000000000, 0x000E000000000000, 0x001C000000000000, 0x0038000000000000, 0x0070000000000000, 0x00E0000000000000, 0x00C0000000000000,
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0] :: V.Vector Bitboard

blackPassedPawnMask = V.fromList [
        0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0,
        0x0000000000000300, 0x0000000000000700, 0x0000000000000E00, 0x0000000000001C00, 0x0000000000003800, 0x0000000000007000, 0x000000000000E000, 0x000000000000C000,
        0x0000000000030300, 0x0000000000070700, 0x00000000000E0E00, 0x00000000001C1C00, 0x0000000000383800, 0x0000000000707000, 0x0000000000E0E000, 0x0000000000C0C000,
        0x0000000003030300, 0x0000000007070700, 0x000000000E0E0E00, 0x000000001C1C1C00, 0x0000000038383800, 0x0000000070707000, 0x00000000E0E0E000, 0x00000000C0C0C000,
        0x0000000303030300, 0x0000000707070700, 0x0000000E0E0E0E00, 0x0000001C1C1C1C00, 0x0000003838383800, 0x0000007070707000, 0x000000E0E0E0E000, 0x000000C0C0C0C000,
        0x0000030303030300, 0x0000070707070700, 0x00000E0E0E0E0E00, 0x00001C1C1C1C1C00, 0x0000383838383800, 0x0000707070707000, 0x0000E0E0E0E0E000, 0x0000C0C0C0C0C000,
        0, 0, 0, 0, 0, 0, 0, 0] :: V.Vector Bitboard

castlePrivWhiteKing :: Bitboard
castlePrivWhiteKing = 1

castlePrivWhiteQueen :: Bitboard
castlePrivWhiteQueen = 2

castlePrivBlackKing :: Bitboard
castlePrivBlackKing = 4

castlePrivBlackQueen :: Bitboard
castlePrivBlackQueen = 8

castlePrivBlackNone :: Bitboard
castlePrivBlackNone = (.&.) (complement castlePrivBlackKing) (complement castlePrivBlackQueen)

castlePrivWhiteNone :: Bitboard
castlePrivWhiteNone = (.&.) (complement castlePrivWhiteKing) (complement castlePrivWhiteQueen)

emptyCastleSquaresWhiteKing :: Bitboard
emptyCastleSquaresWhiteKing = (.|.) (1 `shiftL` 1) (1 `shiftL` 2)

emptyCastleSquaresWhiteQueen :: Bitboard
emptyCastleSquaresWhiteQueen = (.|.) (1 `shiftL` 4) ((.|.) (1 `shiftL` 5) (1 `shiftL` 6))

emptyCastleSquaresBlackKing :: Bitboard
emptyCastleSquaresBlackKing = (.|.) (1 `shiftL` 57) (1 `shiftL` 58)

emptyCastleSquaresBlackQueen :: Bitboard
emptyCastleSquaresBlackQueen = (.|.) (1 `shiftL` 62) ((.|.) (1 `shiftL` 61) (1 `shiftL` 60))

noCheckCastleSquaresWhiteKing :: Bitboard
noCheckCastleSquaresWhiteKing = (.|.) (1 `shiftL` 2) (1 `shiftL` 3)

noCheckCastleSquaresWhiteQueen :: Bitboard
noCheckCastleSquaresWhiteQueen = (.|.) (1 `shiftL` 3) (1 `shiftL` 4)

noCheckCastleSquaresBlackKing :: Bitboard
noCheckCastleSquaresBlackKing = (.|.) (1 `shiftL` 58) (1 `shiftL` 59)

noCheckCastleSquaresBlackQueen :: Bitboard
noCheckCastleSquaresBlackQueen = (.|.) (1 `shiftL` 59) (1 `shiftL` 60)
