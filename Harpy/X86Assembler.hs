{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances,FunctionalDependencies #-}
--------------------------------------------------------------------------
-- |
-- Module      :  Harpy.X86Assembler
-- Copyright   :  (c) 2006-2015 Martin Grabmueller and Dirk Kleeblatt
-- License     :  BSD3
--
-- Maintainer  :  martin@grabmueller.de
-- Stability   :  provisional
-- Portability :  non-portable
--
-- A type class based layer on top of X86CodeGen
-- which determines the addressing modes from the types of the
-- operands.
--------------------------------------------------------------------------
module Harpy.X86Assembler (
   module Harpy.X86Assembler,
   XMMReg(..),
   ) where

import Harpy.X86CodeGen
import Harpy.CodeGenMonad
import Data.Word
import Foreign.Ptr

import qualified Text.PrettyPrint.HughesPJ as PP


-- address modes used in this module:

-- Word8/16/32	   	     immediate values
-- Reg8/16/32	             register
-- Addr Word32		     absolut
-- Ind Reg32		     register indirect
-- (Disp, Reg32)		     register indirect with displacement
-- (Reg32, Reg32, Scale)	     (base, index, scale), effective address is (base + index * scale)
-- (Disp, Reg32, Scale)	     (disp, index, scale), effective address is (disp + index * scale)
-- (Disp, Reg32, Reg32, Scale)  (base, index, scale) + displacement (only ebp is allowed as base register)
-- Label                        not-yet-specified label

onlyEbp = failCodeGen (PP.text "only epb is allowed as base register for disp/base/index/scale addressing")
onlyCl  = failCodeGen (PP.text "only cl is allowed as shift count")


-- x86 Registers

newtype Reg8 = Reg8 Word8
al, cl, dl, bl, ah, ch, dh, bh :: Reg8

al = Reg8 0
cl = Reg8 1
dl = Reg8 2
bl = Reg8 3
ah = Reg8 4
ch = Reg8 5
dh = Reg8 6
bh = Reg8 7

newtype Reg16 = Reg16 Word8
ax, cx, dx, bx, sp, bp, si, di :: Reg16

ax = Reg16 0
cx = Reg16 1
dx = Reg16 2
bx = Reg16 3
sp = Reg16 4
bp = Reg16 5
si = Reg16 6
di = Reg16 7

newtype Reg32 = Reg32 Word8 deriving (Eq, Ord)
eax, ecx, edx, ebx, esp, ebp, esi, edi :: Reg32

eax = Reg32 0
ecx = Reg32 1
edx = Reg32 2
ebx = Reg32 3
esp = Reg32 4
ebp = Reg32 5
esi = Reg32 6
edi = Reg32 7

{-
newtype XMMReg = XMMReg Word8
    deriving (Eq, Ord)
-}

xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 :: XMMReg
xmm0 = XMMReg 0
xmm1 = XMMReg 1
xmm2 = XMMReg 2
xmm3 = XMMReg 3
xmm4 = XMMReg 4
xmm5 = XMMReg 5
xmm6 = XMMReg 6
xmm7 = XMMReg 7

instance Show XMMReg where
    show (XMMReg i) = "xmm" ++ show i

-- TODO: instances for other registers

instance Show Reg32 where
 show (Reg32 0) = "eax"
 show (Reg32 1) = "ecx"
 show (Reg32 2) = "edx"
 show (Reg32 3) = "ebx"
 show (Reg32 4) = "esp"
 show (Reg32 5) = "ebp"
 show (Reg32 6) = "esi"
 show (Reg32 7) = "edi"

-- memory addresses

newtype Addr  = Addr Word32
newtype Ind   = Ind Reg32
newtype Disp  = Disp Word32

data    Scale = S1 | S2 | S4 | S8

scaleToShift :: Scale -> Word8
scaleToShift S1 = 0
scaleToShift S2 = 1
scaleToShift S4 = 2
scaleToShift S8 = 3

newtype FPReg = FPReg Word8

data FPTopReg = FPTopReg

fpTop = FPTopReg

fp0 = FPReg 0
fp1 = FPReg 1
fp2 = FPReg 2
fp3 = FPReg 3
fp4 = FPReg 4
fp5 = FPReg 5
fp6 = FPReg 6
fp7 = FPReg 7

-- int 3

breakpoint = ensureBufferSize x86_max_instruction_bytes >> x86_breakpoint


-- clear direction flag

cld = ensureBufferSize x86_max_instruction_bytes >> x86_cld


-- store string

stosb = ensureBufferSize x86_max_instruction_bytes >> x86_stosb
stosl = ensureBufferSize x86_max_instruction_bytes >> x86_stosl
stosd = ensureBufferSize x86_max_instruction_bytes >> x86_stosd


-- move string

movsb = ensureBufferSize x86_max_instruction_bytes >> x86_movsb
movsl = ensureBufferSize x86_max_instruction_bytes >> x86_movsl
--movsd = ensureBufferSize x86_max_instruction_bytes >> x86_movsd


-- read time stamp counter

rdtsc = ensureBufferSize x86_max_instruction_bytes >> x86_rdtsc


-- compare and exchange

class Cmpxchg a b where
  cmpxchg :: a -> b -> CodeGen e s ()

instance Cmpxchg Reg32 Reg32 where
  cmpxchg (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmpxchg_reg_reg dest source

instance Cmpxchg Addr Reg32 where
  cmpxchg (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmpxchg_mem_reg dest source

instance Cmpxchg (Disp, Reg32) Reg32 where
  cmpxchg (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmpxchg_membase_reg dest disp source

instance Cmpxchg Ind Reg32 where
  cmpxchg (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmpxchg_membase_reg dest 0 source



-- exchange memory/register with register

class Xchg a b where
  xchg :: a -> b -> CodeGen e s ()

instance Xchg Reg8 Reg8 where
  xchg (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_reg_reg dest source 1

instance Xchg Reg32 Reg32 where
  xchg (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_reg_reg dest source 4

instance Xchg Addr Reg8 where
  xchg (Addr dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_mem_reg dest source 1

instance Xchg Addr Reg32 where
  xchg (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_mem_reg dest source 4

instance Xchg (Disp, Reg32) Reg8 where
  xchg (Disp disp, Reg32 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_membase_reg dest disp source 1

instance Xchg Ind Reg8 where
  xchg (Ind (Reg32 dest)) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_membase_reg dest 0 source 1

instance Xchg (Disp, Reg32) Reg32 where
  xchg (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_membase_reg dest disp source 4

instance Xchg Ind Reg32 where
  xchg (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xchg_membase_reg dest 0 source 4


-- exchange and add

class Xadd a b where
  xadd :: a -> b -> CodeGen e s ()

instance Xadd Reg8 Reg8 where
  xadd (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xadd_reg_reg dest source 1

instance Xadd Reg32 Reg32 where
  xadd (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xadd_reg_reg dest source 4

instance Xadd Addr Reg8 where
  xadd (Addr dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xadd_mem_reg dest source 1

instance Xadd Addr Reg32 where
  xadd (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xadd_mem_reg dest source 4

instance Xadd (Disp, Reg32) Reg8 where
  xadd (Disp disp, Reg32 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xadd_membase_reg dest disp source 1

instance Xadd Ind Reg8 where
  xadd (Ind (Reg32 dest)) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_xadd_membase_reg dest 0 source 1

instance Xadd (Disp, Reg32) Reg32 where
  xadd (Disp disp, Reg32 dest) (Reg32 source) =  ensureBufferSize x86_max_instruction_bytes >> x86_xadd_membase_reg dest disp source 4

instance Xadd Ind Reg32 where
  xadd (Ind (Reg32 dest)) (Reg32 source) =  ensureBufferSize x86_max_instruction_bytes >> x86_xadd_membase_reg dest 0 source 4


-- Increment by 1

class Inc a where
  inc :: a -> CodeGen e s ()

instance Inc Addr where
  inc (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_inc_mem dest

instance Inc (Disp, Reg32) where
  inc (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_inc_membase dest disp

instance Inc Ind where
  inc (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_inc_membase dest 0

instance Inc Reg32 where
  inc (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_inc_reg dest


-- Decrement by 1

class Dec a where
  dec :: a -> CodeGen e s ()

instance Dec Addr where
  dec (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_dec_mem dest

instance Dec (Disp, Reg32) where
  dec (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_dec_membase dest disp

instance Dec Ind where
  dec (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_dec_membase dest 0

instance Dec Reg32 where
  dec (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_dec_reg dest


-- One's complement negation

class Not a where
  not :: a -> CodeGen e s ()

instance Not Addr where
  not (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_not_mem dest

instance Not (Disp, Reg32) where
  not (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_not_membase dest disp

instance Not Ind where
  not (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_not_membase dest 0

instance Not Reg32 where
  not (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_not_reg dest


-- Two's complement negation

class Neg a where
  neg :: a -> CodeGen e s ()

instance Neg Addr where
  neg (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_neg_mem dest

instance Neg (Disp, Reg32) where
  neg (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_neg_membase dest disp

instance Neg Ind where
  neg (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_neg_membase dest 0

instance Neg Reg32 where
  neg (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_neg_reg dest


-- No operation

nop = ensureBufferSize x86_max_instruction_bytes >> x86_nop


-- ALU operations

-- Calling "x86_alu_reg8_reg8 _ _ _ *False* *False*" is a little bit hackish: the last two
-- arguments are set to True for the "high byte registers" ah, bh, ch and dh.
-- x86_reg8_emit then sets the 3rd bit in the register number. This bit is set in our
-- encoding anyway to the right value, so we simply skip this part.

class Add a b where
  add :: a -> b -> CodeGen e s ()

instance Add Reg32 Word32 where
  add (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_add dest (fromIntegral imm)

instance Add Addr Word32 where
  add (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_add dest (fromIntegral imm)

instance Add (Disp, Reg32) Word32 where
  add (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_add dest disp (fromIntegral imm)

instance Add Ind Word32 where
  add (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_add dest 0 (fromIntegral imm)

instance Add (Disp, Reg32) Word8 where
  add (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_add dest disp (fromIntegral imm)

instance Add Ind Word8 where
  add (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_add dest 0 (fromIntegral imm)

instance Add Addr Reg32 where
  add (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_add dest source

instance Add (Disp, Reg32) Reg32 where
  add (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_add dest disp source

instance Add Ind Reg32 where
  add (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_add dest 0 source

instance Add Reg32 Reg32 where
  add (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_add dest source

instance Add Reg8 Reg8 where
  add (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_add dest source False False

instance Add Reg32 Addr where
  add (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_add dest source

instance Add Reg32 (Disp, Reg32) where
  add (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_add dest source disp

instance Add Reg32 Ind where
  add (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_add dest source 0


class Or a b where
  or :: a -> b -> CodeGen e s ()

instance Or Reg32 Word32 where
  or (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_or dest (fromIntegral imm)

instance Or Addr Word32 where
  or (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_or dest (fromIntegral imm)

instance Or (Disp, Reg32) Word32 where
  or (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_or dest disp (fromIntegral imm)

instance Or Ind Word32 where
  or (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_or dest 0 (fromIntegral imm)

instance Or (Disp, Reg32) Word8 where
  or (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_or dest disp (fromIntegral imm)

instance Or Ind Word8 where
  or (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_or dest 0 (fromIntegral imm)

instance Or Addr Reg32 where
  or (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_or dest source

instance Or (Disp, Reg32) Reg32 where
  or (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_or dest disp source

instance Or Ind Reg32 where
  or (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_or dest 0 source

instance Or Reg32 Reg32 where
  or (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_or dest source

instance Or Reg8 Reg8 where
  or (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_or dest source False False

instance Or Reg32 Addr where
  or (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_or dest source

instance Or Reg32 (Disp, Reg32) where
  or (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_or dest source disp

instance Or Reg32 Ind where
  or (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_or dest source 0


class Adc a b where
  adc :: a -> b -> CodeGen e s ()

instance Adc Reg32 Word32 where
  adc (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_adc dest (fromIntegral imm)

instance Adc Addr Word32 where
  adc (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_adc dest (fromIntegral imm)

instance Adc (Disp, Reg32) Word32 where
  adc (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_adc dest disp (fromIntegral imm)

instance Adc Ind Word32 where
  adc (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_adc dest 0 (fromIntegral imm)

instance Adc (Disp, Reg32) Word8 where
  adc (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_adc dest disp (fromIntegral imm)

instance Adc Ind Word8 where
  adc (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_adc dest 0 (fromIntegral imm)

instance Adc Addr Reg32 where
  adc (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_adc dest source

instance Adc (Disp, Reg32) Reg32 where
  adc (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_adc dest disp source

instance Adc Ind Reg32 where
  adc (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_adc dest 0 source

instance Adc Reg32 Reg32 where
  adc (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_adc dest source

instance Adc Reg8 Reg8 where
  adc (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_adc dest source False False

instance Adc Reg32 Addr where
  adc (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_adc dest source

instance Adc Reg32 (Disp, Reg32) where
  adc (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_adc dest source disp

instance Adc Reg32 Ind where
  adc (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_adc dest source 0


class Sbb a b where
  sbb :: a -> b -> CodeGen e s ()

instance Sbb Reg32 Word32 where
  sbb (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_sbb dest (fromIntegral imm)

instance Sbb Addr Word32 where
  sbb (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_sbb dest (fromIntegral imm)

instance Sbb (Disp, Reg32) Word32 where
  sbb (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_sbb dest disp (fromIntegral imm)

instance Sbb Ind Word32 where
  sbb (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_sbb dest 0 (fromIntegral imm)

instance Sbb (Disp, Reg32) Word8 where
  sbb (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_sbb dest disp (fromIntegral imm)

instance Sbb Ind Word8 where
  sbb (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_sbb dest 0 (fromIntegral imm)

instance Sbb Addr Reg32 where
  sbb (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_sbb dest source

instance Sbb (Disp, Reg32) Reg32 where
  sbb (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_sbb dest disp source

instance Sbb Ind Reg32 where
  sbb (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_sbb dest 0 source

instance Sbb Reg32 Reg32 where
  sbb (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_sbb dest source

instance Sbb Reg8 Reg8 where
  sbb (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_sbb dest source False False

instance Sbb Reg32 Addr where
  sbb (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_sbb dest source

instance Sbb Reg32 (Disp, Reg32) where
  sbb (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_sbb dest source disp

instance Sbb Reg32 Ind where
  sbb (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_sbb dest source 0


class And a b where
  and :: a -> b -> CodeGen e s ()

instance And Reg32 Word32 where
  and (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_and dest (fromIntegral imm)

instance And Addr Word32 where
  and (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_and dest (fromIntegral imm)

instance And (Disp, Reg32) Word32 where
  and (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_and dest disp (fromIntegral imm)

instance And Ind Word32 where
  and (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_and dest 0 (fromIntegral imm)

instance And (Disp, Reg32) Word8 where
  and (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_and dest disp (fromIntegral imm)

instance And Ind Word8 where
  and (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_and dest 0 (fromIntegral imm)

instance And Addr Reg32 where
  and (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_and dest source

instance And (Disp, Reg32) Reg32 where
  and (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_and dest disp source

instance And Ind Reg32 where
  and (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_and dest 0 source

instance And Reg32 Reg32 where
  and (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_and dest source

instance And Reg8 Reg8 where
  and (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_and dest source False False

instance And Reg32 Addr where
  and (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_and dest source

instance And Reg32 (Disp, Reg32) where
  and (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_and dest source disp

instance And Reg32 Ind where
  and (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_and dest source 0


class Sub a b where
  sub :: a -> b -> CodeGen e s ()

instance Sub Reg32 Word32 where
  sub (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_sub dest (fromIntegral imm)

instance Sub Addr Word32 where
  sub (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_sub dest (fromIntegral imm)

instance Sub (Disp, Reg32) Word32 where
  sub (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_sub dest disp (fromIntegral imm)

instance Sub Ind Word32 where
  sub (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_sub dest 0 (fromIntegral imm)

instance Sub (Disp, Reg32) Word8 where
  sub (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_sub dest disp (fromIntegral imm)

instance Sub Ind Word8 where
  sub (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_sub dest 0 (fromIntegral imm)

instance Sub Addr Reg32 where
  sub (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_sub dest source

instance Sub (Disp, Reg32) Reg32 where
  sub (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_sub dest disp source

instance Sub Ind Reg32 where
  sub (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_sub dest 0 source

instance Sub Reg32 Reg32 where
  sub (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_sub dest source

instance Sub Reg8 Reg8 where
  sub (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_sub dest source False False

instance Sub Reg32 Addr where
  sub (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_sub dest source

instance Sub Reg32 (Disp, Reg32) where
  sub (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_sub dest source disp

instance Sub Reg32 Ind where
  sub (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_sub dest source 0


class Xor a b where
  xor :: a -> b -> CodeGen e s ()

instance Xor Reg32 Word32 where
  xor (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_xor dest (fromIntegral imm)

instance Xor Addr Word32 where
  xor (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_xor dest (fromIntegral imm)

instance Xor (Disp, Reg32) Word32 where
  xor (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_xor dest disp (fromIntegral imm)

instance Xor Ind Word32 where
  xor (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_xor dest 0 (fromIntegral imm)

instance Xor (Disp, Reg32) Word8 where
  xor (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_xor dest disp (fromIntegral imm)

instance Xor Ind Word8 where
  xor (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_xor dest 0 (fromIntegral imm)

instance Xor Addr Reg32 where
  xor (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_xor dest source

instance Xor (Disp, Reg32) Reg32 where
  xor (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_xor dest disp source

instance Xor Ind Reg32 where
  xor (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_xor dest 0 source

instance Xor Reg32 Reg32 where
  xor (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_xor dest source

instance Xor Reg8 Reg8 where
  xor (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_xor dest source False False

instance Xor Reg32 Addr where
  xor (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_xor dest source

instance Xor Reg32 (Disp, Reg32) where
  xor (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_xor dest source disp

instance Xor Reg32 Ind where
  xor (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_xor dest source 0


class Cmp a b where
  cmp :: a -> b -> CodeGen e s ()

instance Cmp Reg32 Word32 where
  cmp (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_cmp dest (fromIntegral imm)

instance Cmp Reg32 (Ptr a) where
  cmp (Reg32 dest) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_imm x86_cmp dest (ptrToInt ptr)

instance Cmp Reg32 Label where
  cmp (Reg32 dest) lab = do
      ensureBufferSize x86_max_instruction_bytes
      x86_alu_reg_imm x86_cmp dest 0xf0f0f0f0
      emitFixup lab (-4) Fixup32Absolute

instance Cmp Addr Word32 where
  cmp (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_cmp dest (fromIntegral imm)

instance Cmp Addr (Ptr a) where
  cmp (Addr dest) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_cmp dest (ptrToWord32 ptr)

instance Cmp Addr Label where
  cmp (Addr dest) lab = do
      ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_imm x86_cmp dest 0xf0f0f0f0
      emitFixup lab (-4) Fixup32Absolute

instance Cmp (Disp, Reg32) Word32 where
  cmp (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_cmp dest disp (fromIntegral imm)

instance Cmp (Disp, Reg32) (Ptr a) where
  cmp (Disp disp, Reg32 dest) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_cmp dest disp (ptrToWord32 ptr)

instance Cmp (Disp, Reg32) Label where
  cmp (Disp disp, Reg32 dest) lab = do
                                   ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_cmp dest disp 0xf0f0f0f0
                                   emitFixup lab (-4) Fixup32Absolute

instance Cmp Ind Word32 where
  cmp (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_cmp dest 0 (fromIntegral imm)

instance Cmp Ind (Ptr a) where
  cmp (Ind (Reg32 dest)) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_cmp dest 0 (ptrToWord32 ptr)

instance Cmp Ind Label where
  cmp (Ind (Reg32 dest)) lab = do
                         ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_imm x86_cmp dest 0 0xf0f0f0f0
                         emitFixup lab (-4) Fixup32Absolute

instance Cmp (Disp, Reg32) Word8 where
  cmp (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_cmp dest disp imm

instance Cmp Ind Word8 where
  cmp (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase8_imm x86_cmp dest 0 imm

instance Cmp Addr Reg32 where
  cmp (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_mem_reg x86_cmp dest source

instance Cmp (Disp, Reg32) Reg32 where
  cmp (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_cmp dest disp source

instance Cmp Ind Reg32 where
  cmp (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_membase_reg x86_cmp dest 0 source

instance Cmp Reg32 Reg32 where
  cmp (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_reg x86_cmp dest source

instance Cmp Reg8 Reg8 where
  cmp (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg8_reg8 x86_cmp dest source False False

instance Cmp Reg32 Addr where
  cmp (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_mem x86_cmp dest source

instance Cmp Reg32 (Disp, Reg32) where
  cmp (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_cmp dest source disp

instance Cmp Reg32 Ind where
  cmp (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_alu_reg_membase x86_cmp dest source 0


-- logical compare

class Test a b where
  test :: a -> b -> CodeGen e s ()

instance Test Reg32 Word32 where
  test (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_test_reg_imm dest imm

instance Test Addr Word32 where
  test (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_test_mem_imm dest imm

instance Test (Disp, Reg32) Word32 where
  test (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_test_membase_imm dest disp imm

instance Test Ind Word32 where
  test (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_test_membase_imm dest 0 imm

instance Test Reg32 Reg32 where
  test (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_test_reg_reg dest source

instance Test Addr Reg32 where
  test (Addr dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_test_mem_reg dest source

instance Test (Disp, Reg32) Reg32 where
  test (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_test_membase_reg dest disp source

instance Test Ind Reg32 where
  test (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_test_membase_reg dest 0 source


-- shift and rotate

class Rol a b where
  rol :: a -> b -> CodeGen e s ()

instance Rol Reg32 Word8 where
  rol (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_rol dest imm

instance Rol Addr Word8 where
  rol (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_rol dest imm

instance Rol (Disp, Reg32) Word8 where
  rol (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_rol dest disp imm

instance Rol Ind Word8 where
  rol (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_rol dest 0 imm

instance Rol Reg32 Reg8 where
  rol (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_rol dest
  rol _ _ = onlyCl

instance Rol Addr Reg8 where
  rol (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_rol dest
  rol _ _ = onlyCl

instance Rol (Disp, Reg32) Reg8 where
  rol (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_rol dest disp

instance Rol Ind Reg8 where
  rol (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_rol dest 0
  rol _ _ = onlyCl

class Ror a b where
  ror :: a -> b -> CodeGen e s ()

instance Ror Reg32 Word8 where
  ror (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_ror dest imm

instance Ror Addr Word8 where
  ror (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_ror dest imm

instance Ror (Disp, Reg32) Word8 where
  ror (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_ror dest disp imm

instance Ror Ind Word8 where
  ror (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_ror dest 0 imm

instance Ror Reg32 Reg8 where
  ror (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_ror dest
  ror _ _ = onlyCl

instance Ror Addr Reg8 where
  ror (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_ror dest
  ror _ _ = onlyCl

instance Ror (Disp, Reg32) Reg8 where
  ror (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_ror dest disp

instance Ror Ind Reg8 where
  ror (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_ror dest 0
  ror _ _ = onlyCl

class Rcl a b where
  rcl :: a -> b -> CodeGen e s ()

instance Rcl Reg32 Word8 where
  rcl (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_rcl dest imm

instance Rcl Addr Word8 where
  rcl (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_rcl dest imm

instance Rcl (Disp, Reg32) Word8 where
  rcl (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_rcl dest disp imm

instance Rcl Ind Word8 where
  rcl (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_rcl dest 0 imm

instance Rcl Reg32 Reg8 where
  rcl (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_rcl dest
  rcl _ _ = onlyCl

instance Rcl Addr Reg8 where
  rcl (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_rcl dest
  rcl _ _ = onlyCl

instance Rcl (Disp, Reg32) Reg8 where
  rcl (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_rcl dest disp

instance Rcl Ind Reg8 where
  rcl (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_rcl dest 0
  rcl _ _ = onlyCl

class Rcr a b where
  rcr :: a -> b -> CodeGen e s ()

instance Rcr Reg32 Word8 where
  rcr (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_rcr dest imm

instance Rcr Addr Word8 where
  rcr (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_rcr dest imm

instance Rcr (Disp, Reg32) Word8 where
  rcr (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_rcr dest disp imm

instance Rcr Ind Word8 where
  rcr (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_rcr dest 0 imm

instance Rcr Reg32 Reg8 where
  rcr (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_rcr dest
  rcr _ _ = onlyCl

instance Rcr Addr Reg8 where
  rcr (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_rcr dest
  rcr _ _ = onlyCl

instance Rcr (Disp, Reg32) Reg8 where
  rcr (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_rcr dest disp

instance Rcr Ind Reg8 where
  rcr (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_rcr dest 0
  rcr _ _ = onlyCl

class Shl a b where
  shl :: a -> b -> CodeGen e s ()

instance Shl Reg32 Word8 where
  shl (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_shl dest imm

instance Shl Addr Word8 where
  shl (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_shl dest imm

instance Shl (Disp, Reg32) Word8 where
  shl (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_shl dest disp imm

instance Shl Ind Word8 where
  shl (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_shl dest 0 imm

instance Shl Reg32 Reg8 where
  shl (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_shl dest
  shl _ _ = onlyCl

instance Shl Addr Reg8 where
  shl (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_shl dest
  shl _ _ = onlyCl

instance Shl (Disp, Reg32) Reg8 where
  shl (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_shl dest disp

instance Shl Ind Reg8 where
  shl (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_shl dest 0
  shl _ _ = onlyCl

class Shr a b where
  shr :: a -> b -> CodeGen e s ()

instance Shr Reg32 Word8 where
  shr (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_shr dest imm

instance Shr Addr Word8 where
  shr (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_shr dest imm

instance Shr (Disp, Reg32) Word8 where
  shr (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_shr dest disp imm

instance Shr Ind Word8 where
  shr (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_shr dest 0 imm

instance Shr Reg32 Reg8 where
  shr (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_shr dest
  shr _ _ = onlyCl

instance Shr Addr Reg8 where
  shr (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_shr dest
  shr _ _ = onlyCl

instance Shr (Disp, Reg32) Reg8 where
  shr (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_shr dest disp

instance Shr Ind Reg8 where
  shr (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_shr dest 0
  shr _ _ = onlyCl

class Sar a b where
  sar :: a -> b -> CodeGen e s ()

instance Sar Reg32 Word8 where
  sar (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_sar dest imm

instance Sar Addr Word8 where
  sar (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_sar dest imm

instance Sar (Disp, Reg32) Word8 where
  sar (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_sar dest disp imm

instance Sar Ind Word8 where
  sar (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_sar dest 0 imm

instance Sar Reg32 Reg8 where
  sar (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_sar dest
  sar _ _ = onlyCl

instance Sar Addr Reg8 where
  sar (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_sar dest
  sar _ _ = onlyCl

instance Sar (Disp, Reg32) Reg8 where
  sar (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_sar dest disp

instance Sar Ind Reg8 where
  sar (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_sar dest 0
  sar _ _ = onlyCl

class Sal a b where
  sal :: a -> b -> CodeGen e s ()

instance Sal Reg32 Word8 where
  sal (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg_imm x86_shl dest imm

instance Sal Addr Word8 where
  sal (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem_imm x86_shl dest imm

instance Sal (Disp, Reg32) Word8 where
  sal (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_shl dest disp imm

instance Sal Ind Word8 where
  sal (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase_imm x86_shl dest 0 imm

instance Sal Reg32 Reg8 where
  sal (Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_reg x86_shl dest
  sal _ _ = onlyCl

instance Sal Addr Reg8 where
  sal (Addr dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_mem x86_shl dest
  sal _ _ = onlyCl

instance Sal (Disp, Reg32) Reg8 where
  sal (Disp disp, Reg32 dest) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_shl dest disp

instance Sal Ind Reg8 where
  sal (Ind (Reg32 dest)) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shift_membase x86_shl dest 0
  sal _ _ = onlyCl


-- double precision shift right

class Shrd a b c where
  shrd :: a -> b -> c -> CodeGen e s ()

instance Shrd Reg32 Reg32 Reg8 where
  shrd (Reg32 dest) (Reg32 source) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shrd_reg dest source
  shrd _ _ _ = onlyCl

instance Shrd Reg32 Reg32 Word8 where
  shrd (Reg32 dest) (Reg32 source) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shrd_reg_imm dest source imm


-- double precision shift left

class Shld a b c where
  shld :: a -> b -> c -> CodeGen e s ()

instance Shld Reg32 Reg32 Reg8 where
  shld (Reg32 dest) (Reg32 source) (Reg8 1) = ensureBufferSize x86_max_instruction_bytes >> x86_shld_reg dest source
  shld _ _ _ = onlyCl

instance Shld Reg32 Reg32 Word8 where
  shld (Reg32 dest) (Reg32 source) imm = ensureBufferSize x86_max_instruction_bytes >> x86_shld_reg_imm dest source imm


-- unsigned multiply

class Mul a where
  mul :: a -> CodeGen e s ()

instance Mul Reg32 where
  mul (Reg32 arg) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_reg arg False

instance Mul Addr where
  mul (Addr arg) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_mem arg False

instance Mul (Disp, Reg32) where
  mul (Disp disp, Reg32 arg) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_membase arg disp False

instance Mul Ind where
  mul (Ind (Reg32 arg)) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_membase arg 0 False


-- signed multiply

data InPlace = InPlace

-- if a == InPlace then
--   b = b * c
-- else
--   a = b * c

class Imul a b c where
  imul :: a -> b -> c -> CodeGen e s ()

instance Imul InPlace Reg32 Reg32 where
  imul _ (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_reg dest source

instance Imul InPlace Reg32 Addr where
  imul _ (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_mem dest source

instance Imul InPlace Reg32 (Disp, Reg32) where
  imul _ (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_membase dest source disp

instance Imul InPlace Reg32 Ind where
  imul _ (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_membase dest source 0

instance Imul Reg32 Reg32 Word32 where
  imul (Reg32 dest) (Reg32 source) imm = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_reg_imm dest source imm

instance Imul Reg32 Addr Word32 where
  imul (Reg32 dest) (Addr source) imm = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_mem_imm dest source imm

instance Imul Reg32 (Disp, Reg32) Word32 where
  imul (Reg32 dest) (Disp disp, Reg32 source) imm = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_membase_imm dest source disp imm

instance Imul Reg32 Ind Word32 where
  imul (Reg32 dest) (Ind (Reg32 source)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_imul_reg_membase_imm dest source 0 imm


-- divide EDX:EAX by rm;
-- eax = quotient, edx = remainder

-- unsigned divide

class Div a where
  div :: a -> CodeGen e s ()

instance Div Reg32 where
  div (Reg32 arg) = ensureBufferSize x86_max_instruction_bytes >> x86_div_reg arg False

instance Div Addr where
  div (Addr arg) = ensureBufferSize x86_max_instruction_bytes >> x86_div_mem arg False

instance Div (Disp, Reg32) where
  div (Disp disp, Reg32 arg) = ensureBufferSize x86_max_instruction_bytes >> x86_div_membase arg disp False

instance Div Ind where
  div (Ind (Reg32 arg)) = ensureBufferSize x86_max_instruction_bytes >> x86_div_membase arg 0 False


-- signed divide

class Idiv a where
  idiv :: a -> CodeGen e s ()

instance Idiv Reg32 where
  idiv (Reg32 arg) = ensureBufferSize x86_max_instruction_bytes >> x86_div_reg arg True

instance Idiv Addr where
  idiv (Addr arg) = ensureBufferSize x86_max_instruction_bytes >> x86_div_mem arg True

instance Idiv (Disp, Reg32) where
  idiv (Disp disp, Reg32 arg) = ensureBufferSize x86_max_instruction_bytes >> x86_div_membase arg disp True

instance Idiv Ind where
  idiv (Ind (Reg32 arg)) = ensureBufferSize x86_max_instruction_bytes >> x86_div_membase arg 0 True


-- "mov" instruction for different sources and destinations

class Mov a b where
  mov :: a -> b -> CodeGen e s ()


instance Mov Reg8 Reg8 where
  mov (Reg8 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_reg dest source 1

instance Mov Reg16 Reg16 where
  mov (Reg16 dest) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_reg dest source 2

instance Mov Reg32 Reg32 where
  mov (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_reg dest source 4


instance Mov Reg32 Word32 where
  mov (Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_imm dest (fromIntegral imm)

instance Mov Reg32 (Ptr a) where
  mov (Reg32 dest) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_imm dest (ptrToWord32 ptr)

instance Mov Reg32 Label where
  mov (Reg32 dest) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_imm dest 0
                            emitFixup lab (-4) Fixup32Absolute

instance Mov Addr Word8 where
  mov (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_imm dest (fromIntegral imm) 1

instance Mov Addr Word16 where
  mov (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_imm dest (fromIntegral imm) 2

instance Mov Addr Word32 where
  mov (Addr dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_imm dest imm 4

instance Mov Addr (Ptr a) where
  mov (Addr dest) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_imm dest (ptrToWord32 ptr) 4

instance Mov Addr Label where
  mov (Addr dest) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_imm dest 0 4
                           emitFixup lab (-4) Fixup32Absolute

instance Mov (Disp, Reg32) Word8 where
  mov (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest disp (fromIntegral imm) 1

instance Mov Ind Word8 where
  mov (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest 0 (fromIntegral imm) 1

instance Mov (Disp, Reg32) Word16 where
  mov (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest disp (fromIntegral imm) 2

instance Mov Ind Word16 where
  mov (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest 0 (fromIntegral imm) 2

instance Mov (Disp, Reg32) Word32 where
  mov (Disp disp, Reg32 dest) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest disp imm 4

instance Mov (Disp, Reg32) (Ptr a) where
  mov (Disp disp, Reg32 dest) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest disp (ptrToWord32 ptr) 4

instance Mov (Disp, Reg32) Label where
  mov (Disp disp, Reg32 dest) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest disp 0 4
                                       emitFixup lab (-4) Fixup32Absolute

instance Mov Ind Word32 where
  mov (Ind (Reg32 dest)) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest 0 imm 4

instance Mov Ind (Ptr a) where
  mov (Ind (Reg32 dest)) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest 0 (ptrToWord32 ptr) 4

instance Mov Ind Label where
  mov (Ind (Reg32 dest)) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_imm dest 0 0 4
                                  emitFixup lab (-4) Fixup32Absolute

instance Mov (Reg32, Reg32, Scale) Word8 where
  mov (Reg32 base, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm base 0 index (scaleToShift scale) (fromIntegral imm) 1

instance Mov (Reg32, Reg32, Scale) Word16 where
  mov (Reg32 base, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm base 0 index (scaleToShift scale) (fromIntegral imm) 2

instance Mov (Reg32, Reg32, Scale) Word32 where
  mov (Reg32 base, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm base 0 index (scaleToShift scale) imm 4

instance Mov (Reg32, Reg32, Scale) (Ptr a) where
  mov (Reg32 base, Reg32 index, scale) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm base 0 index (scaleToShift scale) (ptrToWord32 ptr) 4

instance Mov (Reg32, Reg32, Scale) Label where
  mov (Reg32 base, Reg32 index, scale) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm base 0 index (scaleToShift scale) 0 4
                                                emitFixup lab (-4) Fixup32Absolute

instance Mov (Disp, Reg32, Scale) Word8 where
  mov (Disp disp, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm x86_nobasereg disp index (scaleToShift scale) (fromIntegral imm) 1

instance Mov (Disp, Reg32, Scale) Word16 where
  mov (Disp disp, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm x86_nobasereg disp index (scaleToShift scale) (fromIntegral imm) 2

instance Mov (Disp, Reg32, Scale) Word32 where
  mov (Disp disp, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm x86_nobasereg disp index (scaleToShift scale) imm 4

instance Mov (Disp, Reg32, Scale) (Ptr a) where
  mov (Disp disp, Reg32 index, scale) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm x86_nobasereg disp index (scaleToShift scale) (ptrToWord32 ptr) 4

instance Mov (Disp, Reg32, Scale) Label where
  mov (Disp disp, Reg32 index, scale) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm x86_nobasereg disp index (scaleToShift scale) 0 4
                                               emitFixup lab (-4) Fixup32Absolute

instance Mov (Disp, Reg32, Reg32, Scale) Word8 where
  mov (Disp disp, Reg32 5, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm 5 disp index (scaleToShift scale) (fromIntegral imm) 1
  mov _ _ = onlyEbp

instance Mov (Disp, Reg32, Reg32, Scale) Word16 where
  mov (Disp disp, Reg32 5, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm 5 disp index (scaleToShift scale) (fromIntegral imm) 2
  mov _ _ = onlyEbp

instance Mov (Disp, Reg32, Reg32, Scale) Word32 where
  mov (Disp disp, Reg32 5, Reg32 index, scale) imm = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm 5 disp index (scaleToShift scale) imm 4
  mov _ _ = onlyEbp

instance Mov (Disp, Reg32, Reg32, Scale) (Ptr a) where
  mov (Disp disp, Reg32 5, Reg32 index, scale) ptr = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm 5 disp index (scaleToShift scale) (ptrToWord32 ptr) 4
  mov _ _ = onlyEbp

instance Mov (Disp, Reg32, Reg32, Scale) Label where
  mov (Disp disp, Reg32 5, Reg32 index, scale) lab = do ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_imm 5 disp index (scaleToShift scale) 0 4
                                                        emitFixup lab (-4) Fixup32Absolute
  mov _ _ = onlyEbp

instance Mov Addr Reg8 where
  mov (Addr a) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_reg a source 1

instance Mov Addr Reg16 where
  mov (Addr a) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_reg a source 2

instance Mov Addr Reg32 where
  mov (Addr a) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_mem_reg a source 4

instance Mov Reg8 Addr where
  mov (Reg8 dest) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_mem dest a 1

instance Mov Reg16 Addr where
  mov (Reg16 dest) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_mem dest a 2

instance Mov Reg32 Addr where
  mov (Reg32 dest) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_mem dest a 4


instance Mov Ind Reg8 where
  mov (Ind (Reg32 dest)) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_regp_reg dest source 1

instance Mov Ind Reg16 where
  mov (Ind (Reg32 dest)) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_regp_reg dest source 2

instance Mov Ind Reg32 where
  mov (Ind (Reg32 dest)) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_regp_reg dest source 4

instance Mov Reg8 Ind where
  mov (Reg8 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_regp dest source 1

instance Mov Reg16 Ind where
  mov (Reg16 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_regp dest source 2

instance Mov Reg32 Ind where
  mov (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_regp dest source 4


instance Mov (Disp, Reg32) Reg8 where
  mov (Disp disp, Reg32 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_reg dest disp source 1

instance Mov (Disp, Reg32) Reg16 where
  mov (Disp disp, Reg32 dest) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_reg dest disp source 2

instance Mov (Disp, Reg32) Reg32 where
  mov (Disp disp, Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_membase_reg dest disp source 4

instance Mov Reg8 (Disp, Reg32) where
  mov (Reg8 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_membase dest source disp 1

instance Mov Reg16 (Disp, Reg32) where
  mov (Reg16 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_membase dest source disp 2

instance Mov Reg32 (Disp, Reg32) where
  mov (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_membase dest source disp 4


instance Mov (Reg32, Reg32, Scale) Reg8 where
  mov (Reg32 base, Reg32 index, s) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg base 0 index (scaleToShift s) source 1

instance Mov (Reg32, Reg32, Scale) Reg16 where
  mov (Reg32 base, Reg32 index, s) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg base 0 index (scaleToShift s) source 2

instance Mov (Reg32, Reg32, Scale) Reg32 where
  mov (Reg32 base, Reg32 index, s) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg base 0 index (scaleToShift s) source 4

instance Mov Reg8 (Reg32, Reg32, Scale) where
  mov (Reg8 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest base 0 index (scaleToShift s) 1

instance Mov Reg16 (Reg32, Reg32, Scale) where
  mov (Reg16 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest base 0 index (scaleToShift s) 2

instance Mov Reg32 (Reg32, Reg32, Scale) where
  mov (Reg32 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest base 0 index (scaleToShift s) 4


instance Mov (Disp, Reg32, Scale) Reg8 where
  mov (Disp disp, Reg32 index, s) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg x86_nobasereg disp index (scaleToShift s) source 1

instance Mov (Disp, Reg32, Scale) Reg16 where
  mov (Disp disp, Reg32 index, s) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg x86_nobasereg disp index (scaleToShift s) source 2

instance Mov (Disp, Reg32, Scale) Reg32 where
  mov (Disp disp, Reg32 index, s) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg x86_nobasereg disp index (scaleToShift s) source 4

instance Mov Reg8 (Disp, Reg32, Scale) where
  mov (Reg8 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest x86_nobasereg disp index (scaleToShift s) 1

instance Mov Reg16 (Disp, Reg32, Scale) where
  mov (Reg16 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest x86_nobasereg disp index (scaleToShift s) 2

instance Mov Reg32 (Disp, Reg32, Scale) where
  mov (Reg32 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest x86_nobasereg disp index (scaleToShift s) 4


instance Mov (Disp, Reg32, Reg32, Scale) Reg8 where
  mov (Disp disp, Reg32 5, Reg32 index, s) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg 5 disp index (scaleToShift s) source 1
  mov _ _ = onlyEbp

instance Mov (Disp, Reg32, Reg32, Scale) Reg16 where
  mov (Disp disp, Reg32 5, Reg32 index, s) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg 5 disp index (scaleToShift s) source 2
  mov _ _ = onlyEbp

instance Mov (Disp, Reg32, Reg32, Scale) Reg32 where
  mov (Disp disp, Reg32 5, Reg32 index, s) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> ensureBufferSize x86_max_instruction_bytes >> x86_mov_memindex_reg 5 disp index (scaleToShift s) source 4
  mov _ _ = onlyEbp

instance Mov Reg8 (Disp, Reg32, Reg32, Scale) where
  mov (Reg8 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest 5 disp index (scaleToShift s) 1
  mov _ _ = onlyEbp

instance Mov Reg16 (Disp, Reg32, Reg32, Scale) where
  mov (Reg16 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest 5 disp index (scaleToShift s) 2
  mov _ _ = onlyEbp

instance Mov Reg32 (Disp, Reg32, Reg32, Scale) where
  mov (Reg32 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_reg_memindex dest 5 disp index (scaleToShift s) 4
  mov _ _ = onlyEbp


-- move with sign-extension

class Movsxb a b where
  movsxb :: a -> b -> CodeGen e s ()

instance Movsxb Reg32 Reg8 where
  movsxb (Reg32 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_reg dest source True False

instance Movsxb Reg32 Addr where
  movsxb (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_mem dest source True False

instance Movsxb Reg32 (Disp, Reg32) where
  movsxb (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source disp True False

instance Movsxb Reg32 Ind where
  movsxb (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source 0 True False

instance Movsxb Reg32 (Disp, Reg32, Reg32, Scale) where
  movsxb (Reg32 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest 5 disp index (scaleToShift s) True False
  movsxb _ _ = onlyEbp

instance Movsxb Reg32 (Disp, Reg32, Scale) where
  movsxb (Reg32 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest x86_nobasereg disp index (scaleToShift s) True False

instance Movsxb Reg32 (Reg32, Reg32, Scale) where
  movsxb (Reg32 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest base 0 index (scaleToShift s) True False

class Movsxw a b where
  movsxw :: a -> b -> CodeGen e s ()

instance Movsxw Reg32 Reg16 where
  movsxw (Reg32 dest) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_reg dest source True True

instance Movsxw Reg32 Addr where
  movsxw (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_mem dest source True True

instance Movsxw Reg32 (Disp, Reg32) where
  movsxw (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source disp True True

instance Movsxw Reg32 Ind where
  movsxw (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source 0 True True

instance Movsxw Reg32 (Disp, Reg32, Reg32, Scale) where
  movsxw (Reg32 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest 5 disp index (scaleToShift s) True True
  movsxw _ _ = onlyEbp

instance Movsxw Reg32 (Disp, Reg32, Scale) where
  movsxw (Reg32 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest x86_nobasereg disp index (scaleToShift s) True True

instance Movsxw Reg32 (Reg32, Reg32, Scale) where
  movsxw (Reg32 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest base 0 index (scaleToShift s) True True


-- move with zero-extension

class Movzxb a b where
  movzxb :: a -> b -> CodeGen e s ()

instance Movzxb Reg32 Reg8 where
  movzxb (Reg32 dest) (Reg8 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_reg dest source False False

instance Movzxb Reg32 Addr where
  movzxb (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_mem dest source False False

instance Movzxb Reg32 (Disp, Reg32) where
  movzxb (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source disp False False

instance Movzxb Reg32 Ind where
  movzxb (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source 0 False False

instance Movzxb Reg32 (Disp, Reg32, Reg32, Scale) where
  movzxb (Reg32 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest 5 disp index (scaleToShift s) False False
  movzxb _ _ = onlyEbp

instance Movzxb Reg32 (Disp, Reg32, Scale) where
  movzxb (Reg32 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest x86_nobasereg disp index (scaleToShift s) False False

instance Movzxb Reg32 (Reg32, Reg32, Scale) where
  movzxb (Reg32 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest base 0 index (scaleToShift s) False False

class Movzxw a b where
  movzxw :: a -> b -> CodeGen e s ()

instance Movzxw Reg32 Reg16 where
  movzxw (Reg32 dest) (Reg16 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_reg dest source False True

instance Movzxw Reg32 Addr where
  movzxw (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_mem dest source False True

instance Movzxw Reg32 (Disp, Reg32) where
  movzxw (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source disp False True

instance Movzxw Reg32 Ind where
  movzxw (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_membase dest source 0 False True

instance Movzxw Reg32 (Disp, Reg32, Reg32, Scale) where
  movzxw (Reg32 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest 5 disp index (scaleToShift s) False True
  movzxw _ _ = onlyEbp

instance Movzxw Reg32 (Disp, Reg32, Scale) where
  movzxw (Reg32 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest x86_nobasereg disp index (scaleToShift s) False True

instance Movzxw Reg32 (Reg32, Reg32, Scale) where
  movzxw (Reg32 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_widen_memindex dest base 0 index (scaleToShift s) False True


-- load effective address

class Lea a b where
  lea :: a -> b -> CodeGen e s ()

instance Lea Reg32 Addr where
  lea (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_lea_mem dest source

instance Lea Reg32 (Disp, Reg32) where
  lea (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_lea_membase dest source disp

instance Lea Reg32 Ind where
  lea (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_lea_membase dest source 0

instance Lea Reg32 (Disp, Reg32, Reg32, Scale) where
  lea (Reg32 dest) (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_lea_memindex dest 5 disp index (scaleToShift s)
  lea _ _ = onlyEbp

instance Lea Reg32 (Disp, Reg32, Scale) where
  lea (Reg32 dest) (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_lea_memindex dest x86_nobasereg disp index (scaleToShift s)

instance Lea Reg32 (Reg32, Reg32, Scale) where
  lea (Reg32 dest) (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_lea_memindex dest base 0 index (scaleToShift s)


-- convert word to doubleword

cdq = ensureBufferSize x86_max_instruction_bytes >> x86_cdq


-- wait for FPU

wait = ensureBufferSize x86_max_instruction_bytes >> x86_wait


-- push to stack

class Push a where
  push :: a -> CodeGen e s ()

instance Push Reg32 where
  push (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_push_reg source

instance Push Ind where
  push (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_push_regp source

instance Push Addr where
  push (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_push_mem source

instance Push (Disp, Reg32) where
  push (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_push_membase source disp

instance Push Word32 where
  push imm = ensureBufferSize x86_max_instruction_bytes >> x86_push_imm imm

instance Push Label where
  push l = do ensureBufferSize x86_max_instruction_bytes >> x86_push_imm_template
              emitFixup l (-4) Fixup32Absolute

instance Push (Disp, Reg32, Reg32, Scale) where
  push (Disp disp, Reg32 5, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_push_memindex 5 disp index (scaleToShift s)
  push _ = onlyEbp

instance Push (Disp, Reg32, Scale) where
  push (Disp disp, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_push_memindex x86_nobasereg disp index (scaleToShift s)

instance Push (Reg32, Reg32, Scale) where
  push (Reg32 base, Reg32 index, s) = ensureBufferSize x86_max_instruction_bytes >> x86_push_memindex base 0 index (scaleToShift s)


-- pop from stack

class Pop a where
  pop :: a -> CodeGen e s ()

instance Pop Reg32 where
  pop (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_pop_reg dest

instance Pop Addr where
  pop (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_pop_mem dest

instance Pop (Disp, Reg32) where
  pop (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_pop_membase dest disp

instance Pop Ind where
  pop (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_pop_membase dest 0


-- push/pop general purpose registers

pushad = ensureBufferSize x86_max_instruction_bytes >> x86_pushad
popad  = ensureBufferSize x86_max_instruction_bytes >> x86_popad


-- push/pop EFLAGS

pushfd = ensureBufferSize x86_max_instruction_bytes >> x86_pushfd
popfd  = ensureBufferSize x86_max_instruction_bytes >> x86_popfd


-- loop according to ECX counter

class Loop a where
  loop   :: a -> CodeGen e s ()
  loope  :: a -> CodeGen e s ()
  loopne :: a -> CodeGen e s ()

instance Loop Word8 where
    loop w   = ensureBufferSize x86_max_instruction_bytes >> x86_loop w
    loope w  = ensureBufferSize x86_max_instruction_bytes >> x86_loope w
    loopne w = ensureBufferSize x86_max_instruction_bytes >> x86_loopne w

instance Loop Label where
    loop l   = do ensureBufferSize x86_max_instruction_bytes >> x86_loop 0
                  emitFixup l (-1) Fixup8
    loope l  = do ensureBufferSize x86_max_instruction_bytes >> x86_loope 0
                  emitFixup l (-1) Fixup8
    loopne l = do ensureBufferSize x86_max_instruction_bytes >> x86_loopne 0
                  emitFixup l (-1) Fixup8

-- jump

class Jmp a where
  jmp :: a -> CodeGen e s ()

instance Jmp Word8 where
  jmp imm = ensureBufferSize x86_max_instruction_bytes >> x86_jump8 imm

instance Jmp Word32 where
  jmp imm = ensureBufferSize x86_max_instruction_bytes >> x86_jump32 imm

instance Jmp Reg32 where
  jmp (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_jump_reg dest

instance Jmp Addr where
  jmp (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_jump_mem dest

instance Jmp (Disp, Reg32) where
  jmp (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_jump_membase dest disp

instance Jmp Ind where
  jmp (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_jump_membase dest 0

instance Jmp Label where
  jmp l = do ensureBufferSize x86_max_instruction_bytes >> x86_jump32 0
             emitFixup l (-4) Fixup32

instance Jmp (Ptr a) where
  jmp ptr = ensureBufferSize x86_max_instruction_bytes >> x86_jump_pointer ptr

-- jump on condition code (branch)

class Ja a where
  ja :: a -> CodeGen e s ()

instance Ja Word8 where
  ja imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_a imm False

instance Ja Word32 where
  ja imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_a imm False

instance Ja (Ptr a) where
  ja ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_a ptr False

instance Ja Label where
  ja l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_a 0 False
            emitFixup l (-4) Fixup32

class Jae a where
  jae :: a -> CodeGen e s ()

instance Jae Word8 where
  jae imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_ae imm False

instance Jae Word32 where
  jae imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ae imm False

instance Jae (Ptr a) where
  jae ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_ae ptr False

instance Jae Label where
  jae l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ae 0 False
             emitFixup l (-4) Fixup32

class Jb a where
  jb :: a -> CodeGen e s ()

instance Jb Word8 where
  jb imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_b imm False

instance Jb Word32 where
  jb imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_b imm False

instance Jb (Ptr a) where
  jb ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_b ptr False

instance Jb Label where
  jb l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_b 0 False
            emitFixup l (-4) Fixup32

class Jbe a where
  jbe :: a -> CodeGen e s ()

instance Jbe Word8 where
  jbe imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_be imm False

instance Jbe Word32 where
  jbe imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_be imm False

instance Jbe (Ptr a) where
  jbe ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_be ptr False

instance Jbe Label where
  jbe l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_be 0 False
             emitFixup l (-4) Fixup32

class Jc a where
  jc :: a -> CodeGen e s ()

instance Jc Word8 where
  jc imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_c imm False

instance Jc Word32 where
  jc imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_c imm False

instance Jc (Ptr a) where
  jc ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_c ptr False

instance Jc Label where
  jc l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_c 0 False
            emitFixup l (-4) Fixup32

class Je a where
  je :: a -> CodeGen e s ()

instance Je Word8 where
  je imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_e imm False

instance Je Word32 where
  je imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_e imm False

instance Je (Ptr a) where
  je ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_e ptr False

instance Je Label where
  je l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_e 0 False
            emitFixup l (-4) Fixup32

class Jna a where
  jna :: a -> CodeGen e s ()

instance Jna Word8 where
  jna imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_na imm False

instance Jna Word32 where
  jna imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_na imm False

instance Jna (Ptr a) where
  jna ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_na ptr False

instance Jna Label where
  jna l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_na 0 False
             emitFixup l (-4) Fixup32

class Jnae a where
  jnae :: a -> CodeGen e s ()

instance Jnae Word8 where
  jnae imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_nae imm False

instance Jnae Word32 where
  jnae imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nae imm False

instance Jnae (Ptr a) where
  jnae ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_nae ptr False

instance Jnae Label where
  jnae l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nae 0 False
              emitFixup l (-4) Fixup32

class Jnb a where
  jnb :: a -> CodeGen e s ()

instance Jnb Word8 where
  jnb imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_nb imm False

instance Jnb Word32 where
  jnb imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nb imm False

instance Jnb (Ptr a) where
  jnb ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_nb ptr False

instance Jnb Label where
  jnb l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nb 0 False
             emitFixup l (-4) Fixup32

class Jnbe a where
  jnbe :: a -> CodeGen e s ()

instance Jnbe Word8 where
  jnbe imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_nbe imm False

instance Jnbe Word32 where
  jnbe imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nbe imm False

instance Jnbe (Ptr a) where
  jnbe ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_nbe ptr False

instance Jnbe Label where
  jnbe l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nbe 0 False
              emitFixup l (-4) Fixup32

class Jnc a where
  jnc :: a -> CodeGen e s ()

instance Jnc Word8 where
  jnc imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_nc imm False

instance Jnc Word32 where
  jnc imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nc imm False

instance Jnc (Ptr a) where
  jnc ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_nc ptr False

instance Jnc Label where
  jnc l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nc 0 False
             emitFixup l (-4) Fixup32

class Jne a where
  jne :: a -> CodeGen e s ()

instance Jne Word8 where
  jne imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_ne imm False

instance Jne Word32 where
  jne imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ne imm False

instance Jne (Ptr a) where
  jne ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_ne ptr False

instance Jne Label where
  jne l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ne 0 False
             emitFixup l (-4) Fixup32

class Jnp a where
  jnp :: a -> CodeGen e s ()

instance Jnp Word8 where
  jnp imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_np imm False

instance Jnp Word32 where
  jnp imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_np imm False

instance Jnp (Ptr a) where
  jnp ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_np ptr False

instance Jnp Label where
  jnp l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_np 0 False
             emitFixup l (-4) Fixup32

class Jnz a where
  jnz :: a -> CodeGen e s ()

instance Jnz Word8 where
  jnz imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_nz imm False

instance Jnz Word32 where
  jnz imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nz imm False

instance Jnz (Ptr a) where
  jnz ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_nz ptr False

instance Jnz Label where
  jnz l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_nz 0 False
             emitFixup l (-4) Fixup32

class Jp a where
  jp :: a -> CodeGen e s ()

instance Jp Word8 where
  jp imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_p imm False

instance Jp Word32 where
  jp imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_p imm False

instance Jp (Ptr a) where
  jp ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_p ptr False

instance Jp Label where
  jp l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_p 0 False
            emitFixup l (-4) Fixup32

class Jpe a where
  jpe :: a -> CodeGen e s ()

instance Jpe Word8 where
  jpe imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_pe imm False

instance Jpe Word32 where
  jpe imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_pe imm False

instance Jpe (Ptr a) where
  jpe ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_pe ptr False

instance Jpe Label where
  jpe l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_pe 0 False
             emitFixup l (-4) Fixup32

class Jpo a where
  jpo :: a -> CodeGen e s ()

instance Jpo Word8 where
  jpo imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_po imm False

instance Jpo Word32 where
  jpo imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_po imm False

instance Jpo (Ptr a) where
  jpo ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_po ptr False

instance Jpo Label where
  jpo l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_po 0 False
             emitFixup l (-4) Fixup32

class Jz a where
  jz :: a -> CodeGen e s ()

instance Jz Word8 where
  jz imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_z imm False

instance Jz Word32 where
  jz imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_z imm False

instance Jz (Ptr a) where
  jz ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_z ptr False

instance Jz Label where
  jz l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_z 0 False
            emitFixup l (-4) Fixup32

class Jg a where
  jg :: a -> CodeGen e s ()

instance Jg Word8 where
  jg imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_gt imm True

instance Jg Word32 where
  jg imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_gt imm True

instance Jg (Ptr a) where
  jg ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_gt ptr True

instance Jg Label where
  jg l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_gt 0 True
            emitFixup l (-4) Fixup32

class Jge a where
  jge :: a -> CodeGen e s ()

instance Jge Word8 where
  jge imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_ge imm True

instance Jge Word32 where
  jge imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ge imm True

instance Jge (Ptr a) where
  jge ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_ge ptr True

instance Jge Label where
  jge l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ge 0 True
             emitFixup l (-4) Fixup32

class Jl a where
  jl :: a -> CodeGen e s ()

instance Jl Word8 where
  jl imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_lt imm True

instance Jl Word32 where
  jl imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_lt imm True

instance Jl (Ptr a) where
  jl ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_lt ptr True

instance Jl Label where
  jl l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_lt 0 True
            emitFixup l (-4) Fixup32

class Jle a where
  jle :: a -> CodeGen e s ()

instance Jle Word8 where
  jle imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_le imm True

instance Jle Word32 where
  jle imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_le imm True

instance Jle (Ptr a) where
  jle ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_le ptr True

instance Jle Label where
  jle l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_le 0 True
             emitFixup l (-4) Fixup32

class Jng a where
  jng :: a -> CodeGen e s ()

instance Jng Word8 where
  jng imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_le imm True

instance Jng Word32 where
  jng imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_le imm True

instance Jng (Ptr a) where
  jng ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_le ptr True

instance Jng Label where
  jng l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_le 0 True
             emitFixup l (-4) Fixup32

class Jnge a where
  jnge :: a -> CodeGen e s ()

instance Jnge Word8 where
  jnge imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_lt imm True

instance Jnge Word32 where
  jnge imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_lt imm True

instance Jnge (Ptr a) where
  jnge ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_lt ptr True

instance Jnge Label where
  jnge l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_lt 0 True
              emitFixup l (-4) Fixup32

class Jnl a where
  jnl :: a -> CodeGen e s ()

instance Jnl Word8 where
  jnl imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_ge imm True

instance Jnl Word32 where
  jnl imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ge imm True

instance Jnl (Ptr a) where
  jnl ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_ge ptr True

instance Jnl Label where
  jnl l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ge 0 True
             emitFixup l (-4) Fixup32

class Jnle a where
  jnle :: a -> CodeGen e s ()

instance Jnle Word8 where
  jnle imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_gt imm True

instance Jnle Word32 where
  jnle imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_gt imm True

instance Jnle (Ptr a) where
  jnle ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_gt ptr True

instance Jnle Label where
  jnle l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_gt 0 True
              emitFixup l (-4) Fixup32

class Jno a where
  jno :: a -> CodeGen e s ()

instance Jno Word8 where
  jno imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_no imm True

instance Jno Word32 where
  jno imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_no imm True

instance Jno (Ptr a) where
  jno ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_no ptr True

instance Jno Label where
  jno l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_no 0 True
             emitFixup l (-4) Fixup32

class Jns a where
  jns :: a -> CodeGen e s ()

instance Jns Word8 where
  jns imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_ns imm True

instance Jns Word32 where
  jns imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ns imm True

instance Jns (Ptr a) where
  jns ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_ns ptr True

instance Jns Label where
  jns l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_ns 0 True
             emitFixup l (-4) Fixup32

class Jo a where
  jo :: a -> CodeGen e s ()

instance Jo Word8 where
  jo imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_o imm True

instance Jo Word32 where
  jo imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_o imm True

instance Jo (Ptr a) where
  jo ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_o ptr True

instance Jo Label where
  jo l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_o 0 True
            emitFixup l (-4) Fixup32

class Js a where
  js :: a -> CodeGen e s ()

instance Js Word8 where
  js imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch8 x86_cc_s imm True

instance Js Word32 where
  js imm = ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_s imm True

instance Js (Ptr a) where
  js ptr = ensureBufferSize x86_max_instruction_bytes >> x86_branch_pointer x86_cc_s ptr True

instance Js Label where
  js l = do ensureBufferSize x86_max_instruction_bytes >> x86_branch32 x86_cc_s 0 True
            emitFixup l (-4) Fixup32

-- jump if ecx register is 0

jecxz :: Word8 -> CodeGen e s ()
jecxz w = ensureBufferSize x86_max_instruction_bytes >> x86_jecxz w


-- set byte on condition code

class Seta a where
  seta :: a -> CodeGen e s ()

instance Seta Reg8 where
  seta (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_a dest False

instance Seta Addr where
  seta (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_a dest False

instance Seta (Disp, Reg32) where
  seta (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_a dest disp False

instance Seta Ind where
  seta (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_a dest 0 False

class Setae a where
  setae :: a -> CodeGen e s ()

instance Setae Reg8 where
  setae (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_ae dest False

instance Setae Addr where
  setae (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_ae dest False

instance Setae (Disp, Reg32) where
  setae (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ae dest disp False

instance Setae Ind where
  setae (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ae dest 0 False

class Setb a where
  setb :: a -> CodeGen e s ()

instance Setb Reg8 where
  setb (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_b dest False

instance Setb Addr where
  setb (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_b dest False

instance Setb (Disp, Reg32) where
  setb (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_b dest disp False

instance Setb Ind where
  setb (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_b dest 0 False

class Setbe a where
  setbe :: a -> CodeGen e s ()

instance Setbe Reg8 where
  setbe (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_be dest False

instance Setbe Addr where
  setbe (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_be dest False

instance Setbe (Disp, Reg32) where
  setbe (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_be dest disp False

instance Setbe Ind where
  setbe (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_be dest 0 False

class Setc a where
  setc :: a -> CodeGen e s ()

instance Setc Reg8 where
  setc (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_c dest False

instance Setc Addr where
  setc (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_c dest False

instance Setc (Disp, Reg32) where
  setc (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_c dest disp False

instance Setc Ind where
  setc (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_c dest 0 False

class Sete a where
  sete :: a -> CodeGen e s ()

instance Sete Reg8 where
  sete (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_e dest False

instance Sete Addr where
  sete (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_e dest False

instance Sete (Disp, Reg32) where
  sete (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_e dest disp False

instance Sete Ind where
  sete (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_e dest 0 False

class Setna a where
  setna :: a -> CodeGen e s ()

instance Setna Reg8 where
  setna (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_na dest False

instance Setna Addr where
  setna (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_na dest False

instance Setna (Disp, Reg32) where
  setna (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_na dest disp False

instance Setna Ind where
  setna (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_na dest 0 False

class Setnae a where
  setnae :: a -> CodeGen e s ()

instance Setnae Reg8 where
  setnae (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_nae dest False

instance Setnae Addr where
  setnae (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_nae dest False

instance Setnae (Disp, Reg32) where
  setnae (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nae dest disp False

instance Setnae Ind where
  setnae (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nae dest 0 False

class Setnb a where
  setnb :: a -> CodeGen e s ()

instance Setnb Reg8 where
  setnb (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_nb dest False

instance Setnb Addr where
  setnb (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_nb dest False

instance Setnb (Disp, Reg32) where
  setnb (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nb dest disp False

instance Setnb Ind where
  setnb (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nb dest 0 False

class Setnbe a where
  setnbe :: a -> CodeGen e s ()

instance Setnbe Reg8 where
  setnbe (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_nbe dest False

instance Setnbe Addr where
  setnbe (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_nbe dest False

instance Setnbe (Disp, Reg32) where
  setnbe (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nbe dest disp False

instance Setnbe Ind where
  setnbe (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nbe dest 0 False

class Setnc a where
  setnc :: a -> CodeGen e s ()

instance Setnc Reg8 where
  setnc (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_nc dest False

instance Setnc Addr where
  setnc (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_nc dest False

instance Setnc (Disp, Reg32) where
  setnc (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nc dest disp False

instance Setnc Ind where
  setnc (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nc dest 0 False

class Setne a where
  setne :: a -> CodeGen e s ()

instance Setne Reg8 where
  setne (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_ne dest False

instance Setne Addr where
  setne (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_ne dest False

instance Setne (Disp, Reg32) where
  setne (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ne dest disp False

instance Setne Ind where
  setne (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ne dest 0 False

class Setnp a where
  setnp :: a -> CodeGen e s ()

instance Setnp Reg8 where
  setnp (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_np dest False

instance Setnp Addr where
  setnp (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_np dest False

instance Setnp (Disp, Reg32) where
  setnp (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_np dest disp False

instance Setnp Ind where
  setnp (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_np dest 0 False

class Setnz a where
  setnz :: a -> CodeGen e s ()

instance Setnz Reg8 where
  setnz (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_nz dest False

instance Setnz Addr where
  setnz (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_nz dest False

instance Setnz (Disp, Reg32) where
  setnz (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nz dest disp False

instance Setnz Ind where
  setnz (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_nz dest 0 False

class Setp a where
  setp :: a -> CodeGen e s ()

instance Setp Reg8 where
  setp (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_p dest False

instance Setp Addr where
  setp (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_p dest False

instance Setp (Disp, Reg32) where
  setp (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_p dest disp False

instance Setp Ind where
  setp (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_p dest 0 False

class Setpe a where
  setpe :: a -> CodeGen e s ()

instance Setpe Reg8 where
  setpe (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_pe dest False

instance Setpe Addr where
  setpe (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_pe dest False

instance Setpe (Disp, Reg32) where
  setpe (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_pe dest disp False

instance Setpe Ind where
  setpe (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_pe dest 0 False

class Setpo a where
  setpo :: a -> CodeGen e s ()

instance Setpo Reg8 where
  setpo (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_po dest False

instance Setpo Addr where
  setpo (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_po dest False

instance Setpo (Disp, Reg32) where
  setpo (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_po dest disp False

instance Setpo Ind where
  setpo (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_po dest 0 False

class Setg a where
  setg :: a -> CodeGen e s ()

instance Setg Reg8 where
  setg (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_gt dest True

instance Setg Addr where
  setg (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_gt dest True

instance Setg (Disp, Reg32) where
  setg (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_gt dest disp True

instance Setg Ind where
  setg (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_gt dest 0 True

class Setge a where
  setge :: a -> CodeGen e s ()

instance Setge Reg8 where
  setge (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_ge dest True

instance Setge Addr where
  setge (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_ge dest True

instance Setge (Disp, Reg32) where
  setge (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ge dest disp True

instance Setge Ind where
  setge (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ge dest 0 True

class Setl a where
  setl :: a -> CodeGen e s ()

instance Setl Reg8 where
  setl (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_lt dest True

instance Setl Addr where
  setl (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_lt dest True

instance Setl (Disp, Reg32) where
  setl (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_lt dest disp True

instance Setl Ind where
  setl (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_lt dest 0 True

class Setle a where
  setle :: a -> CodeGen e s ()

instance Setle Reg8 where
  setle (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_le dest True

instance Setle Addr where
  setle (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_le dest True

instance Setle (Disp, Reg32) where
  setle (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_le dest disp True

instance Setle Ind where
  setle (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_le dest 0 True

class Setng a where
  setng :: a -> CodeGen e s ()

instance Setng Reg8 where
  setng (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_le dest True

instance Setng Addr where
  setng (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_le dest True

instance Setng (Disp, Reg32) where
  setng (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_le dest disp True

instance Setng Ind where
  setng (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_le dest 0 True

class Setnge a where
  setnge :: a -> CodeGen e s ()

instance Setnge Reg8 where
  setnge (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_lt dest True

instance Setnge Addr where
  setnge (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_lt dest True

instance Setnge (Disp, Reg32) where
  setnge (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_lt dest disp True

instance Setnge Ind where
  setnge (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_lt dest 0 True

class Setnl a where
  setnl :: a -> CodeGen e s ()

instance Setnl Reg8 where
  setnl (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_ge dest True

instance Setnl Addr where
  setnl (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_ge dest True

instance Setnl (Disp, Reg32) where
  setnl (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ge dest disp True

instance Setnl Ind where
  setnl (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ge dest 0 True

class Setnle a where
  setnle :: a -> CodeGen e s ()

instance Setnle Reg8 where
  setnle (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_gt dest True

instance Setnle Addr where
  setnle (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_gt dest True

instance Setnle (Disp, Reg32) where
  setnle (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_gt dest disp True

instance Setnle Ind where
  setnle (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_gt dest 0 True

class Setno a where
  setno :: a -> CodeGen e s ()

instance Setno Reg8 where
  setno (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_no dest True

instance Setno Addr where
  setno (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_no dest True

instance Setno (Disp, Reg32) where
  setno (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_no dest disp True

instance Setno Ind where
  setno (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_no dest 0 True

class Setns a where
  setns :: a -> CodeGen e s ()

instance Setns Reg8 where
  setns (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_ns dest True

instance Setns Addr where
  setns (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_ns dest True

instance Setns (Disp, Reg32) where
  setns (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ns dest disp True

instance Setns Ind where
  setns (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_ns dest 0 True

class Seto a where
  seto :: a -> CodeGen e s ()

instance Seto Reg8 where
  seto (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_o dest True

instance Seto Addr where
  seto (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_o dest True

instance Seto (Disp, Reg32) where
  seto (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_o dest disp True

instance Seto Ind where
  seto (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_o dest 0 True

class Sets a where
  sets :: a -> CodeGen e s ()

instance Sets Reg8 where
  sets (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_s dest True

instance Sets Addr where
  sets (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_s dest True

instance Sets (Disp, Reg32) where
  sets (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_s dest disp True

instance Sets Ind where
  sets (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_s dest 0 True

class Setz a where
  setz :: a -> CodeGen e s ()

instance Setz Reg8 where
  setz (Reg8 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_reg x86_cc_z dest False

instance Setz Addr where
  setz (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_mem x86_cc_z dest False

instance Setz (Disp, Reg32) where
  setz (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_z dest disp False

instance Setz Ind where
  setz (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_set_membase x86_cc_z dest 0 False


-- call procedure

class Call a where
  call :: a -> CodeGen e s ()

instance Call Word32 where
  call imm = ensureBufferSize x86_max_instruction_bytes >> x86_call_imm imm

instance Call Label where
  call l = do ensureBufferSize x86_max_instruction_bytes >> x86_call_imm 0
              emitFixup l (-4) Fixup32

instance Call Reg32 where
  call (Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_call_reg dest

instance Call Addr where
  call (Addr dest) = ensureBufferSize x86_max_instruction_bytes >> x86_call_mem dest

instance Call (Disp, Reg32) where
  call (Disp disp, Reg32 dest) = ensureBufferSize x86_max_instruction_bytes >> x86_call_membase dest disp

instance Call Ind where
  call (Ind (Reg32 dest)) = ensureBufferSize x86_max_instruction_bytes >> x86_call_membase dest 0

instance Call (FunPtr a) where
  call f = ensureBufferSize x86_max_instruction_bytes >> x86_call_hs f


-- return from procedure

ret :: CodeGen e s ()
ret = ensureBufferSize x86_max_instruction_bytes >> x86_ret

retN :: Word16 -> CodeGen e s ()
retN n = ensureBufferSize x86_max_instruction_bytes >> x86_ret_imm n


-- make stack frame

enter :: Word16 -> CodeGen e s ()
enter w = ensureBufferSize x86_max_instruction_bytes >> x86_enter w


-- conditional move

class Cmova a b where
  cmova :: a -> b -> CodeGen e s ()

instance Cmova Reg32 Reg32 where
  cmova (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_a False dest source

instance Cmova Reg32 Addr where
  cmova (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_a False dest source

instance Cmova Reg32 (Disp, Reg32) where
  cmova (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_a False dest source disp

instance Cmova Reg32 Ind where
  cmova (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_a False dest source 0

class Cmovae a b where
  cmovae :: a -> b -> CodeGen e s ()

instance Cmovae Reg32 Reg32 where
  cmovae (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_ae False dest source

instance Cmovae Reg32 Addr where
  cmovae (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_ae False dest source

instance Cmovae Reg32 (Disp, Reg32) where
  cmovae (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ae False dest source disp

instance Cmovae Reg32 Ind where
  cmovae (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ae False dest source 0

class Cmovb a b where
  cmovb :: a -> b -> CodeGen e s ()

instance Cmovb Reg32 Reg32 where
  cmovb (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_b False dest source

instance Cmovb Reg32 Addr where
  cmovb (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_b False dest source

instance Cmovb Reg32 (Disp, Reg32) where
  cmovb (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_b False dest source disp

instance Cmovb Reg32 Ind where
  cmovb (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_b False dest source 0

class Cmovbe a b where
  cmovbe :: a -> b -> CodeGen e s ()

instance Cmovbe Reg32 Reg32 where
  cmovbe (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_be False dest source

instance Cmovbe Reg32 Addr where
  cmovbe (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_be False dest source

instance Cmovbe Reg32 (Disp, Reg32) where
  cmovbe (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_be False dest source disp

instance Cmovbe Reg32 Ind where
  cmovbe (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_be False dest source 0

class Cmovc a b where
  cmovc :: a -> b -> CodeGen e s ()

instance Cmovc Reg32 Reg32 where
  cmovc (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_c False dest source

instance Cmovc Reg32 Addr where
  cmovc (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_c False dest source

instance Cmovc Reg32 (Disp, Reg32) where
  cmovc (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_c False dest source disp

instance Cmovc Reg32 Ind where
  cmovc (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_c False dest source 0

class Cmove a b where
  cmove :: a -> b -> CodeGen e s ()

instance Cmove Reg32 Reg32 where
  cmove (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_e False dest source

instance Cmove Reg32 Addr where
  cmove (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_e False dest source

instance Cmove Reg32 (Disp, Reg32) where
  cmove (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_e False dest source disp

instance Cmove Reg32 Ind where
  cmove (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_e False dest source 0

class Cmovna a b where
  cmovna :: a -> b -> CodeGen e s ()

instance Cmovna Reg32 Reg32 where
  cmovna (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_na False dest source

instance Cmovna Reg32 Addr where
  cmovna (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_na False dest source

instance Cmovna Reg32 (Disp, Reg32) where
  cmovna (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_na False dest source disp

instance Cmovna Reg32 Ind where
  cmovna (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_na False dest source 0

class Cmovnae a b where
  cmovnae :: a -> b -> CodeGen e s ()

instance Cmovnae Reg32 Reg32 where
  cmovnae (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_nae False dest source

instance Cmovnae Reg32 Addr where
  cmovnae (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_nae False dest source

instance Cmovnae Reg32 (Disp, Reg32) where
  cmovnae (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nae False dest source disp

instance Cmovnae Reg32 Ind where
  cmovnae (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nae False dest source 0

class Cmovnb a b where
  cmovnb :: a -> b -> CodeGen e s ()

instance Cmovnb Reg32 Reg32 where
  cmovnb (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_nb False dest source

instance Cmovnb Reg32 Addr where
  cmovnb (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_nb False dest source

instance Cmovnb Reg32 (Disp, Reg32) where
  cmovnb (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nb False dest source disp

instance Cmovnb Reg32 Ind where
  cmovnb (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nb False dest source 0

class Cmovnbe a b where
  cmovnbe :: a -> b -> CodeGen e s ()

instance Cmovnbe Reg32 Reg32 where
  cmovnbe (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_nbe False dest source

instance Cmovnbe Reg32 Addr where
  cmovnbe (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_nbe False dest source

instance Cmovnbe Reg32 (Disp, Reg32) where
  cmovnbe (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nbe False dest source disp

instance Cmovnbe Reg32 Ind where
  cmovnbe (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nbe False dest source 0

class Cmovnc a b where
  cmovnc :: a -> b -> CodeGen e s ()

instance Cmovnc Reg32 Reg32 where
  cmovnc (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_nc False dest source

instance Cmovnc Reg32 Addr where
  cmovnc (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_nc False dest source

instance Cmovnc Reg32 (Disp, Reg32) where
  cmovnc (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nc False dest source disp

instance Cmovnc Reg32 Ind where
  cmovnc (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nc False dest source 0

class Cmovne a b where
  cmovne :: a -> b -> CodeGen e s ()

instance Cmovne Reg32 Reg32 where
  cmovne (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_ne False dest source

instance Cmovne Reg32 Addr where
  cmovne (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_ne False dest source

instance Cmovne Reg32 (Disp, Reg32) where
  cmovne (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ne False dest source disp

instance Cmovne Reg32 Ind where
  cmovne (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ne False dest source 0

class Cmovnp a b where
  cmovnp :: a -> b -> CodeGen e s ()

instance Cmovnp Reg32 Reg32 where
  cmovnp (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_np False dest source

instance Cmovnp Reg32 Addr where
  cmovnp (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_np False dest source

instance Cmovnp Reg32 (Disp, Reg32) where
  cmovnp (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_np False dest source disp

instance Cmovnp Reg32 Ind where
  cmovnp (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_np False dest source 0

class Cmovnz a b where
  cmovnz :: a -> b -> CodeGen e s ()

instance Cmovnz Reg32 Reg32 where
  cmovnz (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_nz False dest source

instance Cmovnz Reg32 Addr where
  cmovnz (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_nz False dest source

instance Cmovnz Reg32 (Disp, Reg32) where
  cmovnz (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nz False dest source disp

instance Cmovnz Reg32 Ind where
  cmovnz (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_nz False dest source 0

class Cmovp a b where
  cmovp :: a -> b -> CodeGen e s ()

instance Cmovp Reg32 Reg32 where
  cmovp (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_p False dest source

instance Cmovp Reg32 Addr where
  cmovp (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_p False dest source

instance Cmovp Reg32 (Disp, Reg32) where
  cmovp (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_p False dest source disp

instance Cmovp Reg32 Ind where
  cmovp (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_p False dest source 0

class Cmovpe a b where
  cmovpe :: a -> b -> CodeGen e s ()

instance Cmovpe Reg32 Reg32 where
  cmovpe (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_pe False dest source

instance Cmovpe Reg32 Addr where
  cmovpe (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_pe False dest source

instance Cmovpe Reg32 (Disp, Reg32) where
  cmovpe (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_pe False dest source disp

instance Cmovpe Reg32 Ind where
  cmovpe (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_pe False dest source 0

class Cmovpo a b where
  cmovpo :: a -> b -> CodeGen e s ()

instance Cmovpo Reg32 Reg32 where
  cmovpo (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_po False dest source

instance Cmovpo Reg32 Addr where
  cmovpo (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_po False dest source

instance Cmovpo Reg32 (Disp, Reg32) where
  cmovpo (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_po False dest source disp

instance Cmovpo Reg32 Ind where
  cmovpo (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_po False dest source 0

class Cmovz a b where
  cmovz :: a -> b -> CodeGen e s ()

instance Cmovz Reg32 Reg32 where
  cmovz (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_z False dest source

instance Cmovz Reg32 Addr where
  cmovz (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_z False dest source

instance Cmovz Reg32 (Disp, Reg32) where
  cmovz (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_z False dest source disp

instance Cmovz Reg32 Ind where
  cmovz (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_z False dest source 0

class Cmovg a b where
  cmovg :: a -> b -> CodeGen e s ()

instance Cmovg Reg32 Reg32 where
  cmovg (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_gt True dest source

instance Cmovg Reg32 Addr where
  cmovg (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_gt True dest source

instance Cmovg Reg32 (Disp, Reg32) where
  cmovg (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_gt True dest source disp

instance Cmovg Reg32 Ind where
  cmovg (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_gt True dest source 0

class Cmovge a b where
  cmovge :: a -> b -> CodeGen e s ()

instance Cmovge Reg32 Reg32 where
  cmovge (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_ge True dest source

instance Cmovge Reg32 Addr where
  cmovge (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_ge True dest source

instance Cmovge Reg32 (Disp, Reg32) where
  cmovge (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ge True dest source disp

instance Cmovge Reg32 Ind where
  cmovge (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ge True dest source 0

class Cmovl a b where
  cmovl :: a -> b -> CodeGen e s ()

instance Cmovl Reg32 Reg32 where
  cmovl (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_lt True dest source

instance Cmovl Reg32 Addr where
  cmovl (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_lt True dest source

instance Cmovl Reg32 (Disp, Reg32) where
  cmovl (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_lt True dest source disp

instance Cmovl Reg32 Ind where
  cmovl (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_lt True dest source 0

class Cmovle a b where
  cmovle :: a -> b -> CodeGen e s ()

instance Cmovle Reg32 Reg32 where
  cmovle (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_le True dest source

instance Cmovle Reg32 Addr where
  cmovle (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_le True dest source

instance Cmovle Reg32 (Disp, Reg32) where
  cmovle (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_le True dest source disp

instance Cmovle Reg32 Ind where
  cmovle (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_le True dest source 0

class Cmovng a b where
  cmovng :: a -> b -> CodeGen e s ()

instance Cmovng Reg32 Reg32 where
  cmovng (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_le True dest source

instance Cmovng Reg32 Addr where
  cmovng (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_le True dest source

instance Cmovng Reg32 (Disp, Reg32) where
  cmovng (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_le True dest source disp

instance Cmovng Reg32 Ind where
  cmovng (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_le True dest source 0

class Cmovnge a b where
  cmovnge :: a -> b -> CodeGen e s ()

instance Cmovnge Reg32 Reg32 where
  cmovnge (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_lt True dest source

instance Cmovnge Reg32 Addr where
  cmovnge (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_lt True dest source

instance Cmovnge Reg32 (Disp, Reg32) where
  cmovnge (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_lt True dest source disp

instance Cmovnge Reg32 Ind where
  cmovnge (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_lt True dest source 0

class Cmovnl a b where
  cmovnl :: a -> b -> CodeGen e s ()

instance Cmovnl Reg32 Reg32 where
  cmovnl (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_ge True dest source

instance Cmovnl Reg32 Addr where
  cmovnl (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_ge True dest source

instance Cmovnl Reg32 (Disp, Reg32) where
  cmovnl (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ge True dest source disp

instance Cmovnl Reg32 Ind where
  cmovnl (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ge True dest source 0

class Cmovnle a b where
  cmovnle :: a -> b -> CodeGen e s ()

instance Cmovnle Reg32 Reg32 where
  cmovnle (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_gt True dest source

instance Cmovnle Reg32 Addr where
  cmovnle (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_gt True dest source

instance Cmovnle Reg32 (Disp, Reg32) where
  cmovnle (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_gt True dest source disp

instance Cmovnle Reg32 Ind where
  cmovnle (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_gt True dest source 0

class Cmovno a b where
  cmovno :: a -> b -> CodeGen e s ()

instance Cmovno Reg32 Reg32 where
  cmovno (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_no True dest source

instance Cmovno Reg32 Addr where
  cmovno (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_no True dest source

instance Cmovno Reg32 (Disp, Reg32) where
  cmovno (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_no True dest source disp

instance Cmovno Reg32 Ind where
  cmovno (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_no True dest source 0

class Cmovns a b where
  cmovns :: a -> b -> CodeGen e s ()

instance Cmovns Reg32 Reg32 where
  cmovns (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_ns True dest source

instance Cmovns Reg32 Addr where
  cmovns (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_ns True dest source

instance Cmovns Reg32 (Disp, Reg32) where
  cmovns (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ns True dest source disp

instance Cmovns Reg32 Ind where
  cmovns (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_ns True dest source 0

class Cmovo a b where
  cmovo :: a -> b -> CodeGen e s ()

instance Cmovo Reg32 Reg32 where
  cmovo (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_o True dest source

instance Cmovo Reg32 Addr where
  cmovo (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_o True dest source

instance Cmovo Reg32 (Disp, Reg32) where
  cmovo (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_o True dest source disp

instance Cmovo Reg32 Ind where
  cmovo (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_o True dest source 0

class Cmovs a b where
  cmovs :: a -> b -> CodeGen e s ()

instance Cmovs Reg32 Reg32 where
  cmovs (Reg32 dest) (Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_reg x86_cc_s True dest source

instance Cmovs Reg32 Addr where
  cmovs (Reg32 dest) (Addr source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_mem x86_cc_s True dest source

instance Cmovs Reg32 (Disp, Reg32) where
  cmovs (Reg32 dest) (Disp disp, Reg32 source) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_s True dest source disp

instance Cmovs Reg32 Ind where
  cmovs (Reg32 dest) (Ind (Reg32 source)) = ensureBufferSize x86_max_instruction_bytes >> x86_cmov_membase x86_cc_s True dest source 0


-- release stack frame

leave :: CodeGen e s ()
leave = ensureBufferSize x86_max_instruction_bytes >> x86_leave


-- store ah into flags

sahf :: CodeGen e s ()
sahf = ensureBufferSize x86_max_instruction_bytes >> x86_sahf

-- Floating point instructions

fldz = ensureBufferSize x86_max_instruction_bytes >> x86_fldz
fld1 = ensureBufferSize x86_max_instruction_bytes >> x86_fld1
fldpi = ensureBufferSize x86_max_instruction_bytes >> x86_fldpi

fstsw = ensureBufferSize x86_max_instruction_bytes >> x86_fstsw
fnstsw = ensureBufferSize x86_max_instruction_bytes >> x86_fstsw

fcompp = ensureBufferSize x86_max_instruction_bytes >> x86_fcompp
fucompp = ensureBufferSize x86_max_instruction_bytes >> x86_fucompp

fchs = ensureBufferSize x86_max_instruction_bytes >> x86_fchs
frem = ensureBufferSize x86_max_instruction_bytes >> x86_frem

fxch (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fxch idx

fcomi (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fcomi idx
fcomip (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fcomip idx
fucomi (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fucomi idx
fucomip (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fucomip idx

fsin = ensureBufferSize x86_max_instruction_bytes >> x86_fsin
fcos = ensureBufferSize x86_max_instruction_bytes >> x86_fcos
fptan = ensureBufferSize x86_max_instruction_bytes >> x86_fptan
fpatan = ensureBufferSize x86_max_instruction_bytes >> x86_fpatan
fabs = ensureBufferSize x86_max_instruction_bytes >> x86_fabs
ftst = ensureBufferSize x86_max_instruction_bytes >> x86_ftst
fxam = ensureBufferSize x86_max_instruction_bytes >> x86_fxam
fprem = ensureBufferSize x86_max_instruction_bytes >> x86_fprem
fprem1 = ensureBufferSize x86_max_instruction_bytes >> x86_fprem1
frndint = ensureBufferSize x86_max_instruction_bytes >> x86_frndint
fsqrt = ensureBufferSize x86_max_instruction_bytes >> x86_fsqrt

class Fadd a b where
    fadd :: a -> b -> CodeGen e s ()

instance Fadd FPTopReg FPReg where
    fadd FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fadd idx

instance Fadd FPTopReg Addr where
    fadd FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fadd a True

instance Fadd FPTopReg (Disp, Reg32) where
    fadd FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fadd r d True

instance Fadd FPTopReg Ind where
    fadd FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fadd r 0 True

instance Fadd FPReg FPTopReg where
    fadd (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fadd idx False


class Faddp a b where
    faddp :: a -> b -> CodeGen e s ()

instance Faddp FPReg FPTopReg where
    faddp (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fadd idx True


class Fiadd a b where
    fiadd32 :: a -> b -> CodeGen e s ()
    fiadd16 :: a -> b -> CodeGen e s ()

instance Fiadd FPTopReg (Disp, Reg32) where
    fiadd32 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fadd r d True
    fiadd16 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fadd r d False

instance Fiadd FPTopReg Ind where
    fiadd32 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fadd r 0 True
    fiadd16 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fadd r 0 False


class Fsub a b where
    fsub :: a -> b -> CodeGen e s ()

instance Fsub FPTopReg FPReg where
    fsub FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fsub idx

instance Fsub FPTopReg Addr where
    fsub FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fsub a True

instance Fsub FPTopReg (Disp, Reg32) where
    fsub FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fsub r d True

instance Fsub FPTopReg Ind where
    fsub FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fsub r 0 True

instance Fsub FPReg FPTopReg where
    fsub (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fsub idx False


class Fsubp a b where
    fsubp :: a -> b -> CodeGen e s ()

instance Fsubp FPReg FPTopReg where
    fsubp (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fsub idx True


class Fisub a b where
    fisub32 :: a -> b -> CodeGen e s ()
    fisub16 :: a -> b -> CodeGen e s ()

instance Fisub FPTopReg (Disp, Reg32) where
    fisub32 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fsub r d True
    fisub16 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fsub r d False

instance Fisub FPTopReg Ind where
    fisub32 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fsub r 0 True
    fisub16 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fsub r 0 False


class Fsubr a b where
    fsubr :: a -> b -> CodeGen e s ()

instance Fsubr FPTopReg FPReg where
    fsubr FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fsubr idx

instance Fsubr FPTopReg Addr where
    fsubr FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fsubr a True

instance Fsubr FPTopReg (Disp, Reg32) where
    fsubr FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fsubr r d True

instance Fsubr FPTopReg Ind where
    fsubr FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fsubr r 0 True


class Fmul a b where
    fmul :: a -> b -> CodeGen e s ()

instance Fmul FPTopReg FPReg where
    fmul FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fmul idx

instance Fmul FPTopReg Addr where
    fmul FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fmul a True

instance Fmul FPTopReg (Disp, Reg32) where
    fmul FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fmul r d True

instance Fmul FPTopReg Ind where
    fmul FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fmul r 0 True

instance Fmul FPReg FPTopReg where
    fmul (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fmul idx False


class Fmulp a b where
    fmulp :: a -> b -> CodeGen e s ()

instance Fmulp FPReg FPTopReg where
    fmulp (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fmul idx True


class Fimul a b where
    fimul32 :: a -> b -> CodeGen e s ()
    fimul16 :: a -> b -> CodeGen e s ()

instance Fimul FPTopReg (Disp, Reg32) where
    fimul32 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fmul r d True
    fimul16 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fmul r d False

instance Fimul FPTopReg Ind where
    fimul32 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fmul r 0 True
    fimul16 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fmul r 0 False


class Fdiv a b where
    fdiv :: a -> b -> CodeGen e s ()

instance Fdiv FPTopReg FPReg where
    fdiv FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fdiv idx

instance Fdiv FPTopReg Addr where
    fdiv FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fdiv a True

instance Fdiv FPTopReg (Disp, Reg32) where
    fdiv FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fdiv r d True

instance Fdiv FPTopReg Ind where
    fdiv FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fdiv r 0 True

instance Fdiv FPReg FPTopReg where
    fdiv (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fdiv idx False


class Fdivp a b where
    fdivp :: a -> b -> CodeGen e s ()

instance Fdivp FPReg FPTopReg where
    fdivp (FPReg idx) FPTopReg = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_reg x86_fdiv idx True


class Fidiv a b where
    fidiv32 :: a -> b -> CodeGen e s ()
    fidiv16 :: a -> b -> CodeGen e s ()

instance Fidiv FPTopReg (Disp, Reg32) where
    fidiv32 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fdiv r d True
    fidiv16 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fdiv r d False

instance Fidiv FPTopReg Ind where
    fidiv32 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fdiv r 0 True
    fidiv16 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_int_op_membase x86_fdiv r 0 False


class Fdivr a b where
    fdivr :: a -> b -> CodeGen e s ()

instance Fdivr FPTopReg FPReg where
    fdivr FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fdivr idx

instance Fdivr FPTopReg Addr where
    fdivr FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fdivr a True

instance Fdivr FPTopReg (Disp, Reg32) where
    fdivr FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fdivr r d True

instance Fdivr FPTopReg Ind where
    fdivr FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fdivr r 0 True


class Fcom a b where
    fcom :: a -> b -> CodeGen e s ()

instance Fcom FPTopReg FPReg where
    fcom FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fcom idx

instance Fcom FPTopReg Addr where
    fcom FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fcom a True

instance Fcom FPTopReg (Disp, Reg32) where
    fcom FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fcom r d True

instance Fcom FPTopReg Ind where
    fcom FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fcom r 0 True


class Fcomp a b where
    fcomp :: a -> b -> CodeGen e s ()

instance Fcomp FPTopReg FPReg where
    fcomp FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op x86_fcomp idx

instance Fcomp FPTopReg Addr where
    fcomp FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_mem x86_fcomp a True

instance Fcomp FPTopReg (Disp, Reg32) where
    fcomp FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fcomp r d True

instance Fcomp FPTopReg Ind where
    fcomp FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fp_op_membase x86_fcomp r 0 True


class Fld a b where
    fld :: a -> b -> CodeGen e s ()

instance Fld FPTopReg FPReg where
    fld FPTopReg (FPReg idx) = ensureBufferSize x86_max_instruction_bytes >> x86_fld_reg idx

instance Fld FPTopReg Addr where
    fld FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fld a True

instance Fld FPTopReg (Disp, Reg32) where
    fld FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fld_membase r d True

instance Fld FPTopReg Ind where
    fld FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fld_membase r 0 True

class Fld80 a b where
    fld80 :: a -> b -> CodeGen e s ()

instance Fld80 FPTopReg Addr where
    fld80 FPTopReg (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fld80_mem a

instance Fld80 FPTopReg (Disp, Reg32) where
    fld80 FPTopReg (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fld80_membase r d

instance Fld80 FPTopReg Ind where
    fld80 FPTopReg (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fld80_membase r 0

class Fst a where
    fst :: a -> CodeGen e s ()

instance Fst Addr where
    fst (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fst a True False

instance Fst (Disp, Reg32) where
    fst (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fst_membase r d True False

instance Fst Ind where
    fst (Ind (Reg32 r)) =  ensureBufferSize x86_max_instruction_bytes >> x86_fst_membase r 0 True False

class Fstp a where
    fstp :: a -> CodeGen e s ()

instance Fstp FPReg where
    fstp (FPReg r) = ensureBufferSize x86_max_instruction_bytes >> x86_fstp r

instance Fstp Addr where
    fstp (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fst a True True

instance Fstp (Disp, Reg32) where
    fstp (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fst_membase r d True True

instance Fstp Ind where
    fstp (Ind (Reg32 r)) =  ensureBufferSize x86_max_instruction_bytes >> x86_fst_membase r 0 True True


class Fst80 a where
    fst80 :: a -> CodeGen e s ()

instance Fst80 Addr where
    fst80 (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fst80_mem a

instance Fst80 (Disp, Reg32) where
    fst80 (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fst80_membase r d

instance Fst80 Ind where
    fst80 (Ind (Reg32 r)) =  ensureBufferSize x86_max_instruction_bytes >> x86_fst80_membase r 0


class Fnstcw a where
    fnstcw :: a -> CodeGen e s ()

instance Fnstcw Addr where
    fnstcw (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fnstcw a

instance Fnstcw (Disp, Reg32) where
    fnstcw (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fnstcw_membase r d

instance Fnstcw Ind where
    fnstcw (Ind (Reg32 r)) =  ensureBufferSize x86_max_instruction_bytes >> x86_fnstcw_membase r 0


class Fldcw a where
    fldcw :: a -> CodeGen e s ()

instance Fldcw Addr where
    fldcw (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fldcw a

instance Fldcw (Disp, Reg32) where
    fldcw (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fldcw_membase r d

instance Fldcw Ind where
    fldcw (Ind (Reg32 r)) =  ensureBufferSize x86_max_instruction_bytes >> x86_fldcw_membase r 0


class Fild a where
    fild :: a -> CodeGen e s ()

instance Fild Addr where
    fild (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fild a FInt32

instance Fild (Disp, Reg32) where
    fild (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fild_membase r d FInt32

instance Fild Ind where
    fild (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fild_membase r 0 FInt32


class Fist a where
    fist :: a -> CodeGen e s ()

instance Fist (Disp, Reg32) where
    fist (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fist_membase r d FInt32

instance Fist Ind where
    fist (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fist_membase r 0 FInt32


class Fistp a where
    fistp :: a -> CodeGen e s ()

instance Fistp Addr where
    fistp (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_fist_pop a FInt32

instance Fistp (Disp, Reg32) where
    fistp (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_fist_pop_membase r d FInt32

instance Fistp Ind where
    fistp (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_fist_pop_membase r 0 FInt32


class Sqrtsd a b where
    sqrtsd :: a -> b -> CodeGen e s ()

instance Sqrtsd XMMReg XMMReg where
    sqrtsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_reg x86_sse_sd xd xs

instance Sqrtsd XMMReg Addr where
    sqrtsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_mem x86_sse_sd xd a

instance Sqrtsd XMMReg Ind where
    sqrtsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_sd xd r 0

instance Sqrtsd XMMReg (Disp, Reg32) where
    sqrtsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_sd xd r d


class Sqrtss a b where
    sqrtss :: a -> b -> CodeGen e s ()

instance Sqrtss XMMReg XMMReg where
    sqrtss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_reg x86_sse_ss xd xs

instance Sqrtss XMMReg Addr where
    sqrtss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_mem x86_sse_ss xd a

instance Sqrtss XMMReg Ind where
    sqrtss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_ss xd r 0

instance Sqrtss XMMReg (Disp, Reg32) where
    sqrtss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_ss xd r d


class Sqrtpd a b where
    sqrtpd :: a -> b -> CodeGen e s ()

instance Sqrtpd XMMReg XMMReg where
    sqrtpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_reg x86_sse_pd xd xs

instance Sqrtpd XMMReg Addr where
    sqrtpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_mem x86_sse_pd xd a

instance Sqrtpd XMMReg Ind where
    sqrtpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_pd xd r 0

instance Sqrtpd XMMReg (Disp, Reg32) where
    sqrtpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_pd xd r d


class Sqrtps a b where
    sqrtps :: a -> b -> CodeGen e s ()

instance Sqrtps XMMReg XMMReg where
    sqrtps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_reg x86_sse_ps xd xs

instance Sqrtps XMMReg Addr where
    sqrtps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_mem x86_sse_ps xd a

instance Sqrtps XMMReg Ind where
    sqrtps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_ps xd r 0

instance Sqrtps XMMReg (Disp, Reg32) where
    sqrtps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sqrt_sse_reg_membase x86_sse_ps xd r d


class Addsd a b where
    addsd :: a -> b -> CodeGen e s ()

instance Addsd XMMReg XMMReg where
    addsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_reg x86_sse_sd xd xs

instance Addsd XMMReg Addr where
    addsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_mem x86_sse_sd xd a

instance Addsd XMMReg Ind where
    addsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_sd xd r 0

instance Addsd XMMReg (Disp, Reg32) where
    addsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_sd xd r d


class Addss a b where
    addss :: a -> b -> CodeGen e s ()

instance Addss XMMReg XMMReg where
    addss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_reg x86_sse_ss xd xs

instance Addss XMMReg Addr where
    addss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_mem x86_sse_ss xd a

instance Addss XMMReg Ind where
    addss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_ss xd r 0

instance Addss XMMReg (Disp, Reg32) where
    addss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_ss xd r d


class Addpd a b where
    addpd :: a -> b -> CodeGen e s ()

instance Addpd XMMReg XMMReg where
    addpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_reg x86_sse_pd xd xs

instance Addpd XMMReg Addr where
    addpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_mem x86_sse_pd xd a

instance Addpd XMMReg Ind where
    addpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_pd xd r 0

instance Addpd XMMReg (Disp, Reg32) where
    addpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_pd xd r d


class Addps a b where
    addps :: a -> b -> CodeGen e s ()

instance Addps XMMReg XMMReg where
    addps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_reg x86_sse_ps xd xs

instance Addps XMMReg Addr where
    addps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_mem x86_sse_ps xd a

instance Addps XMMReg Ind where
    addps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_ps xd r 0

instance Addps XMMReg (Disp, Reg32) where
    addps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_add_sse_reg_membase x86_sse_ps xd r d

class Subsd a b where
    subsd :: a -> b -> CodeGen e s ()

instance Subsd XMMReg XMMReg where
    subsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_reg x86_sse_sd xd xs

instance Subsd XMMReg Addr where
    subsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_mem x86_sse_sd xd a

instance Subsd XMMReg Ind where
    subsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_sd xd r 0

instance Subsd XMMReg (Disp, Reg32) where
    subsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_sd xd r d


class Subss a b where
    subss :: a -> b -> CodeGen e s ()

instance Subss XMMReg XMMReg where
    subss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_reg x86_sse_ss xd xs

instance Subss XMMReg Addr where
    subss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_mem x86_sse_ss xd a

instance Subss XMMReg Ind where
    subss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_ss xd r 0

instance Subss XMMReg (Disp, Reg32) where
    subss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_ss xd r d


class Subpd a b where
    subpd :: a -> b -> CodeGen e s ()

instance Subpd XMMReg XMMReg where
    subpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_reg x86_sse_pd xd xs

instance Subpd XMMReg Addr where
    subpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_mem x86_sse_pd xd a

instance Subpd XMMReg Ind where
    subpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_pd xd r 0

instance Subpd XMMReg (Disp, Reg32) where
    subpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_pd xd r d


class Subps a b where
    subps :: a -> b -> CodeGen e s ()

instance Subps XMMReg XMMReg where
    subps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_reg x86_sse_ps xd xs

instance Subps XMMReg Addr where
    subps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_mem x86_sse_ps xd a

instance Subps XMMReg Ind where
    subps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_ps xd r 0

instance Subps XMMReg (Disp, Reg32) where
    subps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_sub_sse_reg_membase x86_sse_ps xd r d

class Mulsd a b where
    mulsd :: a -> b -> CodeGen e s ()

instance Mulsd XMMReg XMMReg where
    mulsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_reg x86_sse_sd xd xs

instance Mulsd XMMReg Addr where
    mulsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_mem x86_sse_sd xd a

instance Mulsd XMMReg Ind where
    mulsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_sd xd r 0

instance Mulsd XMMReg (Disp, Reg32) where
    mulsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_sd xd r d


class Mulss a b where
    mulss :: a -> b -> CodeGen e s ()

instance Mulss XMMReg XMMReg where
    mulss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_reg x86_sse_ss xd xs

instance Mulss XMMReg Addr where
    mulss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_mem x86_sse_ss xd a

instance Mulss XMMReg Ind where
    mulss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_ss xd r 0

instance Mulss XMMReg (Disp, Reg32) where
    mulss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_ss xd r d


class Mulpd a b where
    mulpd :: a -> b -> CodeGen e s ()

instance Mulpd XMMReg XMMReg where
    mulpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_reg x86_sse_pd xd xs

instance Mulpd XMMReg Addr where
    mulpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_mem x86_sse_pd xd a

instance Mulpd XMMReg Ind where
    mulpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_pd xd r 0

instance Mulpd XMMReg (Disp, Reg32) where
    mulpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_pd xd r d


class Mulps a b where
    mulps :: a -> b -> CodeGen e s ()

instance Mulps XMMReg XMMReg where
    mulps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_reg x86_sse_ps xd xs

instance Mulps XMMReg Addr where
    mulps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_mem x86_sse_ps xd a

instance Mulps XMMReg Ind where
    mulps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_ps xd r 0

instance Mulps XMMReg (Disp, Reg32) where
    mulps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_mul_sse_reg_membase x86_sse_ps xd r d


class Divsd a b where
    divsd :: a -> b -> CodeGen e s ()

instance Divsd XMMReg XMMReg where
    divsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_reg x86_sse_sd xd xs

instance Divsd XMMReg Addr where
    divsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_mem x86_sse_sd xd a

instance Divsd XMMReg Ind where
    divsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_sd xd r 0

instance Divsd XMMReg (Disp, Reg32) where
    divsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_sd xd r d


class Divss a b where
    divss :: a -> b -> CodeGen e s ()

instance Divss XMMReg XMMReg where
    divss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_reg x86_sse_ss xd xs

instance Divss XMMReg Addr where
    divss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_mem x86_sse_ss xd a

instance Divss XMMReg Ind where
    divss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_ss xd r 0

instance Divss XMMReg (Disp, Reg32) where
    divss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_ss xd r d


class Divpd a b where
    divpd :: a -> b -> CodeGen e s ()

instance Divpd XMMReg XMMReg where
    divpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_reg x86_sse_pd xd xs

instance Divpd XMMReg Addr where
    divpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_mem x86_sse_pd xd a

instance Divpd XMMReg Ind where
    divpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_pd xd r 0

instance Divpd XMMReg (Disp, Reg32) where
    divpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_pd xd r d


class Divps a b where
    divps :: a -> b -> CodeGen e s ()

instance Divps XMMReg XMMReg where
    divps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_reg x86_sse_ps xd xs

instance Divps XMMReg Addr where
    divps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_mem x86_sse_ps xd a

instance Divps XMMReg Ind where
    divps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_ps xd r 0

instance Divps XMMReg (Disp, Reg32) where
    divps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_div_sse_reg_membase x86_sse_ps xd r d

class Minsd a b where
    minsd :: a -> b -> CodeGen e s ()

instance Minsd XMMReg XMMReg where
    minsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_reg x86_sse_sd xd xs

instance Minsd XMMReg Addr where
    minsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_mem x86_sse_sd xd a

instance Minsd XMMReg Ind where
    minsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_sd xd r 0

instance Minsd XMMReg (Disp, Reg32) where
    minsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_sd xd r d


class Minss a b where
    minss :: a -> b -> CodeGen e s ()

instance Minss XMMReg XMMReg where
    minss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_reg x86_sse_ss xd xs

instance Minss XMMReg Addr where
    minss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_mem x86_sse_ss xd a

instance Minss XMMReg Ind where
    minss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_ss xd r 0

instance Minss XMMReg (Disp, Reg32) where
    minss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_ss xd r d


class Minpd a b where
    minpd :: a -> b -> CodeGen e s ()

instance Minpd XMMReg XMMReg where
    minpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_reg x86_sse_pd xd xs

instance Minpd XMMReg Addr where
    minpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_mem x86_sse_pd xd a

instance Minpd XMMReg Ind where
    minpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_pd xd r 0

instance Minpd XMMReg (Disp, Reg32) where
    minpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_pd xd r d


class Minps a b where
    minps :: a -> b -> CodeGen e s ()

instance Minps XMMReg XMMReg where
    minps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_reg x86_sse_ps xd xs

instance Minps XMMReg Addr where
    minps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_mem x86_sse_ps xd a

instance Minps XMMReg Ind where
    minps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_ps xd r 0

instance Minps XMMReg (Disp, Reg32) where
    minps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_min_sse_reg_membase x86_sse_ps xd r d


class Maxsd a b where
    maxsd :: a -> b -> CodeGen e s ()

instance Maxsd XMMReg XMMReg where
    maxsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_reg x86_sse_sd xd xs

instance Maxsd XMMReg Addr where
    maxsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_mem x86_sse_sd xd a

instance Maxsd XMMReg Ind where
    maxsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_sd xd r 0

instance Maxsd XMMReg (Disp, Reg32) where
    maxsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_sd xd r d


class Maxss a b where
    maxss :: a -> b -> CodeGen e s ()

instance Maxss XMMReg XMMReg where
    maxss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_reg x86_sse_ss xd xs

instance Maxss XMMReg Addr where
    maxss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_mem x86_sse_ss xd a

instance Maxss XMMReg Ind where
    maxss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_ss xd r 0

instance Maxss XMMReg (Disp, Reg32) where
    maxss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_ss xd r d


class Maxpd a b where
    maxpd :: a -> b -> CodeGen e s ()

instance Maxpd XMMReg XMMReg where
    maxpd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_reg x86_sse_pd xd xs

instance Maxpd XMMReg Addr where
    maxpd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_mem x86_sse_pd xd a

instance Maxpd XMMReg Ind where
    maxpd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_pd xd r 0

instance Maxpd XMMReg (Disp, Reg32) where
    maxpd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_pd xd r d


class Maxps a b where
    maxps :: a -> b -> CodeGen e s ()

instance Maxps XMMReg XMMReg where
    maxps (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_reg x86_sse_ps xd xs

instance Maxps XMMReg Addr where
    maxps (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_mem x86_sse_ps xd a

instance Maxps XMMReg Ind where
    maxps (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_ps xd r 0

instance Maxps XMMReg (Disp, Reg32) where
    maxps (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_max_sse_reg_membase x86_sse_ps xd r d


class Movss a b where
    movss :: a -> b -> CodeGen e s ()

instance Movss XMMReg XMMReg where
    movss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_reg x86_sse_ss xd xs

instance Movss XMMReg Addr where
    movss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_mem x86_sse_ss xd a

instance Movss Addr XMMReg where
    movss (Addr a) (XMMReg xd) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_mem_reg x86_sse_ss a xd

instance Movss XMMReg Ind where
    movss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_membase x86_sse_ss xd r 0

instance Movss Ind XMMReg where
    movss (Ind (Reg32 r)) (XMMReg xd) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_membase_reg x86_sse_ss r 0 xd

instance Movss XMMReg (Disp, Reg32) where
    movss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_membase x86_sse_ss xd r d

instance Movss (Disp, Reg32) XMMReg where
    movss (Disp d, Reg32 r) (XMMReg xd) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_membase_reg x86_sse_ss r d xd


class Movsd a b where
    movsd :: a -> b -> CodeGen e s ()

instance Movsd XMMReg XMMReg where
    movsd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_reg x86_sse_sd xd xs

instance Movsd XMMReg Addr where
    movsd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_mem x86_sse_sd xd a

instance Movsd Addr XMMReg where
    movsd (Addr a) (XMMReg xd) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_mem_reg x86_sse_sd a xd

instance Movsd XMMReg Ind where
    movsd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_membase x86_sse_sd xd r 0

instance Movsd Ind XMMReg where
    movsd (Ind (Reg32 r)) (XMMReg xd) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_membase_reg x86_sse_sd r 0 xd

instance Movsd XMMReg (Disp, Reg32) where
    movsd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_reg_membase x86_sse_sd xd r d

instance Movsd (Disp, Reg32) XMMReg where
    movsd (Disp d, Reg32 r) (XMMReg xd) = ensureBufferSize x86_max_instruction_bytes >> x86_mov_sse_membase_reg x86_sse_sd r d xd


class Movups a b where
    movups :: a -> b -> CodeGen e s ()

instance Movups XMMReg XMMReg where
    movups (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movups_to_reg xd (xmmLocLowLevel xs)

instance Movups XMMReg Addr where
    movups (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movups_to_reg xd (xmmLocLowLevel xs)

instance Movups Addr XMMReg where
    movups xd (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_movups_from_reg xs (xmmLocLowLevel xd)

instance Movups XMMReg Ind where
    movups (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movups_to_reg xd (xmmLocLowLevel xs)

instance Movups Ind XMMReg where
    movups xd (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_movups_from_reg xs (xmmLocLowLevel xd)

instance Movups XMMReg (Disp, Reg32) where
    movups (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movups_to_reg xd (xmmLocLowLevel xs)

instance Movups (Disp, Reg32) XMMReg where
    movups xd (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_movups_from_reg xs (xmmLocLowLevel xd)


class Movlps a b where
    movlps :: a -> b -> CodeGen e s ()

instance Movlps XMMReg XMMReg where
    movlps (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_to_reg xd (xmmLocLowLevel xs)

instance Movlps XMMReg Addr where
    movlps (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_to_reg xd (xmmLocLowLevel xs)

instance Movlps Addr XMMReg where
    movlps xd (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_from_reg xs (xmmLocLowLevel xd)

instance Movlps XMMReg Ind where
    movlps (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_to_reg xd (xmmLocLowLevel xs)

instance Movlps Ind XMMReg where
    movlps xd (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_from_reg xs (xmmLocLowLevel xd)

instance Movlps XMMReg (Disp, Reg32) where
    movlps (XMMReg xd) xs = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_to_reg xd (xmmLocLowLevel xs)

instance Movlps (Disp, Reg32) XMMReg where
    movlps xd (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_movlps_from_reg xs (xmmLocLowLevel xd)


class Comisd a b where
    comisd :: a -> b -> CodeGen e s ()

instance Comisd XMMReg XMMReg where
    comisd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_comisd_reg_reg xd xs

instance Comisd XMMReg Addr where
    comisd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_comisd_reg_mem xd a

instance Comisd XMMReg Ind where
    comisd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_comisd_reg_membase xd r 0

instance Comisd XMMReg (Disp, Reg32) where
    comisd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_comisd_reg_membase xd r d


class Comiss a b where
    comiss :: a -> b -> CodeGen e s ()

instance Comiss XMMReg XMMReg where
    comiss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_comiss_reg_reg xd xs

instance Comiss XMMReg Addr where
    comiss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_comiss_reg_mem xd a

instance Comiss XMMReg Ind where
    comiss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_comiss_reg_membase xd r 0

instance Comiss XMMReg (Disp, Reg32) where
    comiss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_comiss_reg_membase xd r d


class Ucomisd a b where
    ucomisd :: a -> b -> CodeGen e s ()

instance Ucomisd XMMReg XMMReg where
    ucomisd (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomisd_reg_reg xd xs

instance Ucomisd XMMReg Addr where
    ucomisd (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomisd_reg_mem xd a

instance Ucomisd XMMReg Ind where
    ucomisd (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomisd_reg_membase xd r 0

instance Ucomisd XMMReg (Disp, Reg32) where
    ucomisd (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomisd_reg_membase xd r d


class Ucomiss a b where
    ucomiss :: a -> b -> CodeGen e s ()

instance Ucomiss XMMReg XMMReg where
    ucomiss (XMMReg xd) (XMMReg xs) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomiss_reg_reg xd xs

instance Ucomiss XMMReg Addr where
    ucomiss (XMMReg xd) (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomiss_reg_mem xd a

instance Ucomiss XMMReg Ind where
    ucomiss (XMMReg xd) (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomiss_reg_membase xd r 0

instance Ucomiss XMMReg (Disp, Reg32) where
    ucomiss (XMMReg xd) (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_ucomiss_reg_membase xd r d


class XMMLocation b => XMMLoc a b | a -> b where
    xmmLocLowLevel :: a -> b

instance XMMLoc XMMReg XMMReg where
    xmmLocLowLevel = id

instance XMMLoc Addr Mem where
    xmmLocLowLevel (Addr a) = Mem a

instance XMMLoc Ind MemBase where
    xmmLocLowLevel (Ind (Reg32 r)) = MemBase r 0

instance XMMLoc (Disp, Reg32) MemBase where
    xmmLocLowLevel (Disp d, Reg32 r) = MemBase r d


haddps :: XMMLoc xmm a => XMMReg -> xmm -> CodeGen e s ()
haddps (XMMReg dreg) reg =
   x86_haddps dreg (xmmLocLowLevel reg)

haddpd :: XMMLoc xmm a => XMMReg -> xmm -> CodeGen e s ()
haddpd (XMMReg dreg) reg =
   x86_haddpd dreg (xmmLocLowLevel reg)


shufps :: XMMLoc xmm a => XMMReg -> xmm -> Word8 -> CodeGen e s ()
shufps (XMMReg dreg) reg src =
   x86_shufps dreg (xmmLocLowLevel reg) src

shufpd :: XMMLoc xmm a => XMMReg -> xmm -> Word8 -> CodeGen e s ()
shufpd (XMMReg dreg) reg src =
   x86_shufpd dreg (xmmLocLowLevel reg) src


cvtdq2ps :: XMMLoc xmm a => XMMReg -> xmm -> CodeGen e s ()
cvtdq2ps (XMMReg dreg) reg =
   x86_cvtdq2ps dreg (xmmLocLowLevel reg)

cvttps2dq :: XMMLoc xmm a => XMMReg -> xmm -> CodeGen e s ()
cvttps2dq (XMMReg dreg) reg =
   x86_cvttps2dq dreg (xmmLocLowLevel reg)


class Prefetchnta a where
    prefetchnta :: a -> CodeGen e s ()

instance Prefetchnta Addr where
    prefetchnta (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetchnta_mem a

instance Prefetchnta (Disp, Reg32) where
    prefetchnta (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetchnta_membase r d

instance Prefetchnta Ind where
    prefetchnta (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetchnta_regp r

class Prefetch0 a where
    prefetch0 :: a -> CodeGen e s ()

instance Prefetch0 Addr where
    prefetch0 (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch0_mem a

instance Prefetch0 (Disp, Reg32) where
    prefetch0 (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch0_membase r d

instance Prefetch0 Ind where
    prefetch0 (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch0_regp r

class Prefetch1 a where
    prefetch1 :: a -> CodeGen e s ()

instance Prefetch1 Addr where
    prefetch1 (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch1_mem a

instance Prefetch1 (Disp, Reg32) where
    prefetch1 (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch1_membase r d

instance Prefetch1 Ind where
    prefetch1 (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch1_regp r

class Prefetch2 a where
    prefetch2 :: a -> CodeGen e s ()

instance Prefetch2 Addr where
    prefetch2 (Addr a) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch2_mem a

instance Prefetch2 (Disp, Reg32) where
    prefetch2 (Disp d, Reg32 r) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch2_membase r d

instance Prefetch2 Ind where
    prefetch2 (Ind (Reg32 r)) = ensureBufferSize x86_max_instruction_bytes >> x86_prefetch2_regp r


ptrToWord32 :: Ptr a -> Word32
ptrToWord32 = fromIntegral . ptrToIntPtr

ptrToInt :: Ptr a -> Int
ptrToInt = fromIntegral . ptrToIntPtr
