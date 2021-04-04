module Util.Bitboards where

import Data.Bits
import Types

orWithURightShiftedSelf :: Bitboard -> Int -> Bitboard
orWithURightShiftedSelf x y = (.|.) x (shiftR x y)

orWithULeftShiftedSelf :: Bitboard -> Int -> Bitboard
orWithULeftShiftedSelf x y = (.|.) x (shiftL x y)

southFill :: Bitboard -> Bitboard
southFill x = orWithURightShiftedSelf (orWithURightShiftedSelf (orWithURightShiftedSelf x 8) 16) 32

northFill :: Bitboard -> Bitboard
northFill x = orWithULeftShiftedSelf (orWithULeftShiftedSelf (orWithULeftShiftedSelf x 8) 16) 32

everyEighthBitFrom :: Bitboard -> Bitboard
everyEighthBitFrom x = if x < 8 then shiftL 1 x else (.|.) (shiftL 1 x) (everyEighthBitFrom ((-) x 8))

setBits :: [Int] -> Bitboard
setBits [] = 0
setBits [x] = shiftL 1 x
setBits xs = (.|.) (shiftL 1 (head xs)) (setBits (tail xs))

all64BitsSet = 18446744073709551615 :: Int

rank1Bits = setBits [0,1,2,3,4,5,6,7]
rank2Bits = shiftL rank1Bits 8
rank3Bits = shiftL rank2Bits 8
rank4Bits = shiftL rank3Bits 8
rank5Bits = shiftL rank4Bits 8
rank6Bits = shiftL rank5Bits 8
rank7Bits = shiftL rank6Bits 8
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

a1Bit = 7 :: Bitboard
b1Bit = 6 :: Bitboard
c1Bit = 5 :: Bitboard
d1Bit = 4 :: Bitboard
e1Bit = 3 :: Bitboard
f1Bit = 2 :: Bitboard
g1Bit = 1 :: Bitboard
h1Bit = 0 :: Bitboard

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

darkSquaresBits = setBits [a1Bit,a3Bit,a5Bit,a7Bit,b2Bit,b4Bit,b6Bit,b8Bit,c1Bit,c3Bit,c5Bit,c7Bit,d2Bit,d4Bit,d6Bit,d8Bit,e1Bit,e3Bit,e5Bit,e7Bit,f2Bit,f4Bit,f6Bit,f8Bit,g1Bit,g3Bit,g5Bit,g7Bit,h2Bit,h4Bit,h6Bit,h8Bit]
lightSquaresBits = setBits [a2Bit,a4Bit,a6Bit,a8Bit,b1Bit,b3Bit,b5Bit,b7Bit,c2Bit,c4Bit,c6Bit,c8Bit,d1Bit,d3Bit,d5Bit,d7Bit,e2Bit,e4Bit,e6Bit,e8Bit,f1Bit,f3Bit,f5Bit,f7Bit,g2Bit,g4Bit,g6Bit,g8Bit,h1Bit,h3Bit,h5Bit,h7Bit]

knightMovesBitboards = [0x20400,
               0x50800, 0xa1100, 0x142200, 0x284400, 0x508800, 0xa01000, 0x402000, 0x2040004,
               0x5080008, 0xa110011, 0x14220022, 0x28440044, 0x50880088, 0xa0100010, 0x40200020, 0x204000402,
               0x508000805, 0xa1100110a, 0x1422002214, 0x2844004428, 0x5088008850, 0xa0100010a0, 0x4020002040, 0x20400040200,
               0x50800080500, 0xa1100110a00, 0x142200221400, 0x284400442800, 0x508800885000, 0xa0100010a000, 0x402000204000, 0x2040004020000,
               0x5080008050000, 0xa1100110a0000, 0x14220022140000, 0x28440044280000, 0x50880088500000, 0xa0100010a00000, 0x40200020400000, 0x204000402000000,
               0x508000805000000, 0xa1100110a000000, 0x1422002214000000, 0x2844004428000000, 0x5088008850000000, -0x5fefffef60000000, 0x4020002040000000, 0x400040200000000,
               0x800080500000000, 0x1100110a00000000, 0x2200221400000000, 0x4400442800000000, -0x77ff77b000000000, 0x100010a000000000, 0x2000204000000000, 0x4020000000000,
               0x8050000000000, 0x110a0000000000, 0x22140000000000, 0x44280000000000, 0x88500000000000, 0x10a00000000000, 0x20400000000000] :: [Bitboard]

kingMovesBitboards = [
                0x302,
                0x705, 0xe0a, 0x1c14, 0x3828, 0x7050, 0xe0a0, 0xc040, 0x30203,
                0x70507, 0xe0a0e, 0x1c141c, 0x382838, 0x705070, 0xe0a0e0, 0xc040c0, 0x3020300,
                0x7050700, 0xe0a0e00, 0x1c141c00, 0x38283800, 0x70507000, 0xe0a0e000, 0xc040c000, 0x302030000,
                0x705070000, 0xe0a0e0000, 0x1c141c0000, 0x3828380000, 0x7050700000, 0xe0a0e00000, 0xc040c00000, 0x30203000000,
                0x70507000000, 0xe0a0e000000, 0x1c141c000000, 0x382838000000, 0x705070000000, 0xe0a0e0000000, 0xc040c0000000, 0x3020300000000,
                0x7050700000000, 0xe0a0e00000000, 0x1c141c00000000, 0x38283800000000, 0x70507000000000, 0xe0a0e000000000, 0xc040c000000000, 0x302030000000000,
                0x705070000000000, 0xe0a0e0000000000, 0x1c141c0000000000, 0x3828380000000000, 0x7050700000000000, -0x1f5f200000000000, -0x3fbf400000000000, 0x203000000000000,
                0x507000000000000, 0xa0e000000000000, 0x141c000000000000, 0x2838000000000000, 0x5070000000000000, -0x5f20000000000000, 0x40c0000000000000] :: [Bitboard]
