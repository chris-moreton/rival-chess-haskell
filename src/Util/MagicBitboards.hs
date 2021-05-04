{-# LANGUAGE StrictData,BangPatterns #-}

{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Util.MagicBitboards where

import Util.MagicMovesBishop
import Util.MagicMovesRook
import Types
import Alias
import Data.Array.Unboxed
import qualified Data.Vector.Storable as V

occupancyMaskRook :: Int -> Bitboard
occupancyMaskRook 0 = 0x101010101017e
occupancyMaskRook 1 = 0x202020202027c
occupancyMaskRook 2 = 0x404040404047a
occupancyMaskRook 3 = 0x8080808080876
occupancyMaskRook 4 = 0x1010101010106e
occupancyMaskRook 5 = 0x2020202020205e
occupancyMaskRook 6 = 0x4040404040403e
occupancyMaskRook 7 = 0x8080808080807e
occupancyMaskRook 8 = 0x1010101017e00
occupancyMaskRook 9 = 0x2020202027c00
occupancyMaskRook 10 = 0x4040404047a00
occupancyMaskRook 11 = 0x8080808087600
occupancyMaskRook 12 = 0x10101010106e00
occupancyMaskRook 13 = 0x20202020205e00
occupancyMaskRook 14 = 0x40404040403e00
occupancyMaskRook 15 = 0x80808080807e00
occupancyMaskRook 16 = 0x10101017e0100
occupancyMaskRook 17 = 0x20202027c0200
occupancyMaskRook 18 = 0x40404047a0400
occupancyMaskRook 19 = 0x8080808760800
occupancyMaskRook 20 = 0x101010106e1000
occupancyMaskRook 21 = 0x202020205e2000
occupancyMaskRook 22 = 0x404040403e4000
occupancyMaskRook 23 = 0x808080807e8000
occupancyMaskRook 24 = 0x101017e010100
occupancyMaskRook 25 = 0x202027c020200
occupancyMaskRook 26 = 0x404047a040400
occupancyMaskRook 27 = 0x8080876080800
occupancyMaskRook 28 = 0x1010106e101000
occupancyMaskRook 29 = 0x2020205e202000
occupancyMaskRook 30 = 0x4040403e404000
occupancyMaskRook 31 = 0x8080807e808000
occupancyMaskRook 32 = 0x1017e01010100
occupancyMaskRook 33 = 0x2027c02020200
occupancyMaskRook 34 = 0x4047a04040400
occupancyMaskRook 35 = 0x8087608080800
occupancyMaskRook 36 = 0x10106e10101000
occupancyMaskRook 37 = 0x20205e20202000
occupancyMaskRook 38 = 0x40403e40404000
occupancyMaskRook 39 = 0x80807e80808000
occupancyMaskRook 40 = 0x17e0101010100
occupancyMaskRook 41 = 0x27c0202020200
occupancyMaskRook 42 = 0x47a0404040400
occupancyMaskRook 43 = 0x8760808080800
occupancyMaskRook 44 = 0x106e1010101000
occupancyMaskRook 45 = 0x205e2020202000
occupancyMaskRook 46 = 0x403e4040404000
occupancyMaskRook 47 = 0x807e8080808000
occupancyMaskRook 48 = 0x7e010101010100
occupancyMaskRook 49 = 0x7c020202020200
occupancyMaskRook 50 = 0x7a040404040400
occupancyMaskRook 51 = 0x76080808080800
occupancyMaskRook 52 = 0x6e101010101000
occupancyMaskRook 53 = 0x5e202020202000
occupancyMaskRook 54 = 0x3e404040404000
occupancyMaskRook 55 = 0x7e808080808000
occupancyMaskRook 56 = 0x7e01010101010100
occupancyMaskRook 57 = 0x7c02020202020200
occupancyMaskRook 58 = 0x7a04040404040400
occupancyMaskRook 59 = 0x7608080808080800
occupancyMaskRook 60 = 0x6e10101010101000
occupancyMaskRook 61 = 0x5e20202020202000
occupancyMaskRook 62 = 0x3e40404040404000
occupancyMaskRook 63 = 0x7e80808080808000

occupancyMaskBishop :: Int -> Bitboard

occupancyMaskBishop 0 = 0x40201008040200
occupancyMaskBishop 1 = 0x402010080400
occupancyMaskBishop 2 = 0x4020100a00
occupancyMaskBishop 3 = 0x40221400
occupancyMaskBishop 4 = 0x2442800
occupancyMaskBishop 5 = 0x204085000
occupancyMaskBishop 6 = 0x20408102000
occupancyMaskBishop 7 = 0x2040810204000
occupancyMaskBishop 8 = 0x20100804020000
occupancyMaskBishop 9 = 0x40201008040000
occupancyMaskBishop 10 = 0x4020100a0000
occupancyMaskBishop 11 = 0x4022140000
occupancyMaskBishop 12 = 0x244280000
occupancyMaskBishop 13 = 0x20408500000
occupancyMaskBishop 14 = 0x2040810200000
occupancyMaskBishop 15 = 0x4081020400000
occupancyMaskBishop 16 = 0x10080402000200
occupancyMaskBishop 17 = 0x20100804000400
occupancyMaskBishop 18 = 0x4020100a000a00
occupancyMaskBishop 19 = 0x402214001400
occupancyMaskBishop 20 = 0x24428002800
occupancyMaskBishop 21 = 0x2040850005000
occupancyMaskBishop 22 = 0x4081020002000
occupancyMaskBishop 23 = 0x8102040004000
occupancyMaskBishop 24 = 0x8040200020400
occupancyMaskBishop 25 = 0x10080400040800
occupancyMaskBishop 26 = 0x20100a000a1000
occupancyMaskBishop 27 = 0x40221400142200
occupancyMaskBishop 28 = 0x2442800284400
occupancyMaskBishop 29 = 0x4085000500800
occupancyMaskBishop 30 = 0x8102000201000
occupancyMaskBishop 31 = 0x10204000402000
occupancyMaskBishop 32 = 0x4020002040800
occupancyMaskBishop 33 = 0x8040004081000
occupancyMaskBishop 34 = 0x100a000a102000
occupancyMaskBishop 35 = 0x22140014224000
occupancyMaskBishop 36 = 0x44280028440200
occupancyMaskBishop 37 = 0x8500050080400
occupancyMaskBishop 38 = 0x10200020100800
occupancyMaskBishop 39 = 0x20400040201000
occupancyMaskBishop 40 = 0x2000204081000
occupancyMaskBishop 41 = 0x4000408102000
occupancyMaskBishop 42 = 0xa000a10204000
occupancyMaskBishop 43 = 0x14001422400000
occupancyMaskBishop 44 = 0x28002844020000
occupancyMaskBishop 45 = 0x50005008040200
occupancyMaskBishop 46 = 0x20002010080400
occupancyMaskBishop 47 = 0x40004020100800
occupancyMaskBishop 48 = 0x20408102000
occupancyMaskBishop 49 = 0x40810204000
occupancyMaskBishop 50 = 0xa1020400000
occupancyMaskBishop 51 = 0x142240000000
occupancyMaskBishop 52 = 0x284402000000
occupancyMaskBishop 53 = 0x500804020000
occupancyMaskBishop 54 = 0x201008040200
occupancyMaskBishop 55 = 0x402010080400
occupancyMaskBishop 56 = 0x2040810204000
occupancyMaskBishop 57 = 0x4081020400000
occupancyMaskBishop 58 = 0xa102040000000
occupancyMaskBishop 59 = 0x14224000000000
occupancyMaskBishop 60 = 0x28440200000000
occupancyMaskBishop 61 = 0x50080402000000
occupancyMaskBishop 62 = 0x20100804020000
occupancyMaskBishop 63 = 0x40201008040200

magicNumberRook = listArray(0,63) [fromIntegral (-0x5e7ffddf7fbffdd0) :: Word, 0x40100040022000, 0x80088020001002, 0x80080280841000, 0x4200042010460008, 0x4800a0003040080, 0x400110082041008, 0x8000a041000880, 0x10138001a080c010, 0x804008200480, 0x10011012000c0, 0x22004128102200, 0x200081201200c, 0x202a001048460004, 0x81000100420004, 0x4000800380004500, 0x208002904001, 0x90004040026008, 0x208808010002001, 0x2002020020704940, fromIntegral (-0x7fb7fefff7eefffb) :: Word, 0x6820808004002200, 0xa80040008023011, 0xb1460000811044, 0x4204400080008ea0, fromIntegral (-0x4ffdbffe7fdffe7c) :: Word, 0x2020200080100380, 0x10080080100080, 0x2204080080800400, 0xa40080360080, 0x2040604002810b1, 0x8c218600004104, fromIntegral (-0x7e7fffbfffbfe000) :: Word, 0x488c402000401001, 0x4018a00080801004, 0x1230002105001008, fromIntegral (-0x76fb7ff7ff7ffc00) :: Word, 0x42000c42003810, 0x8408110400b012, 0x18086182000401, 0x2240088020c28000, 0x1001201040c004, 0xa02008010420020, 0x10003009010060, 0x4008008008014, 0x80020004008080, 0x282020001008080, 0x50000181204a0004, 0x102042111804200, 0x40002010004001c0, 0x19220045508200, 0x20030010060a900, 0x8018028040080, 0x88240002008080, 0x10301802830400, 0x332a4081140200, 0x8080010a601241, 0x1008010400021, 0x4082001007241, 0x211009001200509, fromIntegral (-0x7feaffeffdbbe7ff) :: Word, 0x801000804000603, 0xc0900220024a401, 0x1000200608243] :: BitboardArray
magicNumberBishop = listArray(0,63) [0x2910054208004104, 0x2100630a7020180, 0x5822022042000000, 0x2ca804a100200020, 0x204042200000900, 0x2002121024000002, fromIntegral (-0x7fbfbefbdfdfff18) :: Word, fromIntegral (-0x7ed5fdfdfafef7c0) :: Word, fromIntegral (-0x7ffae7ee7bf7ffb8) :: Word, 0x1001c20208010101, 0x1001080204002100, 0x181080489021800, 0x62040420010a00, 0x5028043004300020, fromIntegral (-0x3ff7f5bbfd9faffe) :: Word, 0x8a00a0104220200, 0x940000410821212, 0x1808024a280210, 0x40c0422080a0598, 0x4228020082004050, 0x200800400e00100, 0x20b001230021040, 0x90a0201900c00, 0x4940120a0a0108, 0x20208050a42180, 0x1004804b280200, 0x2048020024040010, 0x102c04004010200, 0x20408204c002010, 0x2411100020080c1, 0x102a008084042100, 0x941030000a09846, 0x244100800400200, 0x4000901010080696, 0x280404180020, 0x800042008240100, 0x220008400088020, 0x4020182000904c9, 0x23010400020600, 0x41040020110302, 0x412101004020818, -0x7fddf7f5f6bfbdf8, 0x1401210240484800, 0x22244208010080, 0x1105040104000210, 0x2040088800c40081, fromIntegral (-0x7e7b7efdadfffc00) :: Word, 0x4004610041002200, 0x40201a444400810, 0x4611010802020008, fromIntegral (-0x7ffff4fbfefbfbfe) :: Word, 0x20004821880a00, fromIntegral(-0x7dffffdfddbbff00) :: Word, 0x9431801010068, 0x1040c20806108040, 0x804901403022a40, 0x2400202602104000, 0x208520209440204, 0x40c000022013020, 0x2000104000420600, 0x400000260142410, 0x800633408100500, 0x2404080a1410, 0x138200122002900] :: BitboardArray
magicNumberShiftsRook = listArray(0,63) [52, 53, 53, 53, 53, 53, 53, 52, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 52, 53, 53, 53, 53, 53, 53, 52] :: UArray Int Int
magicNumberShiftsBishop = listArray(0,63) [58, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 58] :: UArray Int Int

magic :: MagicVars -> Square -> Int -> Bitboard
magic !magicVars !fromSquare !toSquaresMagicIndex = magicMoves magicVars V.! ((fromSquare * magicsPerSquare magicVars) + toSquaresMagicIndex)

data MagicVars = MagicVars {
      occupancyMask :: Int -> Bitboard
    , magicNumber :: BitboardArray
    , magicNumberShifts :: UArray Int Int
    , magicMoves :: !MagicMoves
    , magicsPerSquare :: Int
}

magicRookVars = MagicVars {
      occupancyMask = occupancyMaskRook
    , magicNumber = magicNumberRook
    , magicNumberShifts = magicNumberShiftsRook
    , magicMoves = magicMovesRook
    , magicsPerSquare = 4096
  }

magicBishopVars = MagicVars {
      occupancyMask = occupancyMaskBishop
    , magicNumber = magicNumberBishop
    , magicNumberShifts = magicNumberShiftsBishop
    , magicMoves = magicMovesBishop
    , magicsPerSquare = 1024
  }