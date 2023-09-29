--------------------------------------------------------------------------
-- |
-- Module      :  X86CodeGen
-- Copyright   :  (c) 2006-2015 Martin Grabmueller and Dirk Kleeblatt
-- License     :  BSD3
--
-- Maintainer  :  martin@grabmueller.de
-- Stability   :  provisional
-- Portability :  portable (but generated code non-portable)
--
-- Functions for generating x86 machine code instructions.  The
-- functions make use of the code generation monad in module
-- "Harpy.CodeGenMonad" for emitting binary code into a code buffer.
--
-- This module is very low-level, since there are different
-- functions for different addressing modes.  A more convenient
-- interface is provided in module "Harpy.X86Assembler", which uses
-- the operand types to determine the correct addressing modes for
-- all supported instructions.
--
-- Note: this file does not (yet) provide the complete x86
-- instruction set, not even all user-mode instructions.  For some
-- operations, some addressing modes are missing as well.
--
-- Copyright notice:
--
-- The information in this file is based on the header file
-- x86-codegen.h from the mono distribution, which has the following
-- copyright information:
--
-- @
--  * x86-codegen.h: Macros for generating x86 code
--  *
--  * Authors:
--  *   Paolo Molaro (lupus\@ximian.com)
--  *   Intel Corporation (ORP Project)
--  *   Sergey Chaban (serge\@wildwestsoftware.com)
--  *   Dietmar Maurer (dietmar\@ximian.com)
--  *   Patrik Torstensson
--  *
--  * Copyright (C)  2000 Intel Corporation.  All rights reserved.
--  * Copyright (C)  2001, 2002 Ximian, Inc.
--  *
-- @
--------------------------------------------------------------------------

module Harpy.X86CodeGen(
    -- * Types
    X86_SSE_PFX,
    -- * Constants
    -- ** Machine characteristics
    -- |  Sizes of various machine data types in bytes.
    x86_dword_size,
    x86_qword_size,
    x86_max_instruction_bytes,
    -- ** Register numbers
    -- | x86 general-purpose register numbers
    x86_eax, x86_ecx, x86_edx, x86_ebx, x86_esp, x86_ebp, x86_esi, x86_edi,
    x86_nobasereg,
    -- ** Register masks and predicates
    -- | Bitvector masks for general-purpose registers
    x86_eax_mask, x86_ecx_mask, x86_edx_mask, x86_ebx_mask,
    x86_esi_mask, x86_edi_mask, x86_ebp_mask,
    x86_callee_regs, x86_caller_regs, x86_byte_regs,
    -- ** ALU operations
    -- | Opcodes for ALU instructions
    x86_add, x86_or, x86_adc, x86_sbb, x86_and, x86_sub, x86_xor, x86_cmp,
    -- ** Shift operations
    -- | Opcodes for shift instructions
    x86_rol, x86_ror, x86_rcl, x86_rcr, x86_shl,
    x86_shr, x86_sar, x86_shld, x86_shlr,
    -- ** FP operations
    -- | Opcodes for floating-point instructions
    x86_fadd, x86_fmul, x86_fcom, x86_fcomp, x86_fsub, x86_fsubr,
    x86_fdiv, x86_fdivr,
    -- ** FP conditions and control codes
    -- | FP status word codes
    x86_fp_c0, x86_fp_c1, x86_fp_c2, x86_fp_c3, x86_fp_cc_mask,
    -- | FP control word codes
    x86_fpcw_invopex_mask, x86_fpcw_denopex_mask, x86_fpcw_zerodiv_mask,
    x86_fpcw_ovfex_mask, x86_fpcw_undfex_mask, x86_fpcw_precex_mask,
    x86_fpcw_precc_mask, x86_fpcw_roundc_mask,
    x86_fpcw_prec_single, x86_fpcw_prec_double,
    x86_fpcw_prec_extended,
    x86_fpcw_round_nearest, x86_fpcw_round_down, x86_fpcw_round_up,
    x86_fpcw_round_tozero,
    -- ** Condition codes
    -- | Integer conditions codes
    x86_cc_eq, x86_cc_e, x86_cc_z,
    x86_cc_ne, x86_cc_nz,
    x86_cc_lt, x86_cc_b, x86_cc_c, x86_cc_nae, x86_cc_le, x86_cc_be,
    x86_cc_na, x86_cc_gt, x86_cc_a, x86_cc_nbe, x86_cc_ge, x86_cc_ae,
    x86_cc_nb, x86_cc_nc, x86_cc_lz, x86_cc_s, x86_cc_gez, x86_cc_ns,
    x86_cc_p, x86_cc_np, x86_cc_pe, x86_cc_po, x86_cc_o, x86_cc_no,
    -- ** Instruction prefix codes
    x86_lock_prefix, x86_repnz_prefix, x86_repz_prefix, x86_rep_prefix,
    x86_cs_prefix, x86_ss_prefix, x86_ds_prefix, x86_es_prefix,
    x86_fs_prefix, x86_gs_prefix, x86_unlikely_prefix,
    x86_likely_prefix, x86_operand_prefix, x86_address_prefix,
    -- * Functions
    -- ** Utility functions
    x86_is_scratch, x86_is_callee,
    -- ** Code emission
    -- | These functions are used to emit parts of instructions, such
    -- as constants or operand descriptions.
    x86_imm_emit16, x86_imm_emit8, x86_imm_emit32,
    x86_membase_emit, x86_alu_reg_imm,
    -- ** Call instructions
    x86_call_hs, x86_call_membase, x86_call_mem, x86_call_reg, x86_call_code,
    x86_call_imm,
    -- ** Function prologue and epilogue
    x86_prolog, x86_epilog, x86_enter, x86_leave,
    x86_ret, x86_ret_imm,
    -- ** Jump and branch
    x86_jecxz, x86_branch, x86_branch_pointer, x86_branch32, x86_branch8,
    x86_jump_membase, x86_jump_pointer, x86_jump_mem, x86_jump_reg,
    x86_jump32, x86_jump8,
    x86_loopne, x86_loope, x86_loop,
    -- ** Stack operations
    x86_push_reg, x86_push_regp, x86_push_mem, x86_push_membase,
    x86_push_imm, x86_push_imm_template, x86_push_memindex,
    x86_pop_membase, x86_pop_mem, x86_pop_reg,
    x86_popfd, x86_pushfd, x86_popad, x86_pushad,
    -- ** Data movement
    x86_mov_reg_reg, x86_mov_reg_imm, x86_mov_mem_imm, x86_mov_membase_imm,
    x86_mov_memindex_imm, x86_mov_mem_reg, x86_mov_reg_mem,
    x86_mov_regp_reg, x86_mov_reg_regp, x86_mov_membase_reg,
    x86_mov_reg_membase, x86_mov_memindex_reg, x86_mov_reg_memindex,
    -- ** Arithmetic
    x86_xadd_reg_reg, x86_xadd_mem_reg, x86_xadd_membase_reg,
    x86_inc_mem, x86_inc_membase, x86_inc_reg,
    x86_dec_mem, x86_dec_membase, x86_dec_reg,
    x86_not_mem, x86_not_membase, x86_not_reg,
    x86_neg_mem, x86_neg_membase, x86_neg_reg,
    x86_alu_mem_imm, x86_alu_membase_imm, x86_alu_membase8_imm,
    x86_alu_mem_reg, x86_alu_membase_reg, x86_alu_reg_reg,
    x86_alu_reg8_reg8, x86_alu_reg_mem, x86_alu_reg_membase,
    x86_mul_reg, x86_mul_mem, x86_mul_membase,
    x86_imul_reg_reg, x86_imul_reg_membase, x86_imul_reg_reg_imm,
    x86_imul_reg_mem,
    x86_imul_reg_mem_imm, x86_imul_reg_membase_imm,
    x86_div_reg, x86_div_mem, x86_div_membase,
    x86_test_reg_imm, x86_test_mem_imm, x86_test_membase_imm,
    x86_test_reg_reg, x86_test_mem_reg, x86_test_membase_reg,
    -- ** Exchange
    x86_cmpxchg_reg_reg, x86_cmpxchg_mem_reg, x86_cmpxchg_membase_reg,
    x86_xchg_reg_reg, x86_xchg_mem_reg, x86_xchg_membase_reg,
    -- ** String operations
    x86_stosb, x86_stosl, x86_stosd, x86_movsb, x86_movsl, x86_movsd,
    -- ** Bitwise shift
    x86_shift_reg_imm, x86_shift_mem_imm, x86_shift_membase_imm,
    x86_shift_reg, x86_shift_mem, x86_shift_membase,
    x86_shrd_reg, x86_shrd_reg_imm, x86_shld_reg, x86_shld_reg_imm,
    -- ** Conditional move
    x86_cmov_membase, x86_cmov_mem, x86_cmov_reg,
    -- ** Conditional set
    x86_set_membase, x86_set_mem, x86_set_reg,
    -- ** Address calculation
    x86_lea_mem, x86_lea_membase, x86_lea_memindex,
    -- ** Conversion
    x86_cdq,x86_widen_memindex, x86_widen_membase, x86_widen_mem,
    x86_widen_reg,
    -- ** Floating point
    x86_fp_op_mem, x86_fp_op_membase, x86_fp_op, x86_fp_op_reg,
    x86_fp_int_op_membase, x86_fstp, x86_fcompp, x86_fucompp,
    x86_fnstsw, x86_fnstcw, x86_fnstcw_membase,
    x86_fldcw, x86_fldcw_membase, x86_fchs,
    x86_frem, x86_fxch, x86_fcomi, x86_fcomip, x86_fucomi, x86_fucomip,
    x86_fld, x86_fld_membase, x86_fld80_mem, x86_fld80_membase,
    x86_fld_reg, x86_fldz, x86_fld1, x86_fldpi,
    x86_fst, x86_fst_membase, x86_fst80_mem, x86_fst80_membase,
    FIntSize(..),
    x86_fist_pop, x86_fist_pop_membase, x86_fstsw,
    x86_fist_membase, x86_fild, x86_fild_membase,
    x86_fsin, x86_fcos, x86_fabs, x86_ftst, x86_fxam, x86_fpatan,
    x86_fprem, x86_fprem1, x86_frndint, x86_fsqrt, x86_fptan,
    x86_fincstp, x86_fdecstp,
    -- ** SSE instructions
    x86_sse_ps, x86_sse_pd, x86_sse_ss, x86_sse_sd,
    x86_add_sse_reg_reg, x86_add_sse_reg_mem, x86_add_sse_reg_membase,
    x86_sub_sse_reg_reg, x86_sub_sse_reg_mem, x86_sub_sse_reg_membase,
    x86_mul_sse_reg_reg, x86_mul_sse_reg_mem, x86_mul_sse_reg_membase,
    x86_div_sse_reg_reg, x86_div_sse_reg_mem, x86_div_sse_reg_membase,
    x86_max_sse_reg_reg, x86_max_sse_reg_mem, x86_max_sse_reg_membase,
    x86_min_sse_reg_reg, x86_min_sse_reg_mem, x86_min_sse_reg_membase,
    x86_sqrt_sse_reg_reg, x86_sqrt_sse_reg_mem, x86_sqrt_sse_reg_membase,
    x86_mov_sse_reg_reg, x86_mov_sse_reg_mem, x86_mov_sse_reg_membase, x86_mov_sse_mem_reg ,x86_mov_sse_membase_reg,
    x86_ucomisd_reg_reg, x86_ucomisd_reg_mem, x86_ucomisd_reg_membase,
    x86_ucomiss_reg_reg, x86_ucomiss_reg_mem, x86_ucomiss_reg_membase,
    x86_comisd_reg_reg, x86_comisd_reg_mem, x86_comisd_reg_membase,
    x86_comiss_reg_reg, x86_comiss_reg_mem, x86_comiss_reg_membase,
    XMMReg(XMMReg), Mem(Mem), MemBase(MemBase),
    XMMLocation(xmm_location_emit),
    x86_movss_to_reg, x86_movss_from_reg,
    x86_movsd_to_reg, x86_movsd_from_reg,
    x86_movlps_to_reg, x86_movlps_from_reg,
    x86_movlpd_to_reg, x86_movlpd_from_reg,
    x86_movups_to_reg, x86_movups_from_reg,
    x86_movupd_to_reg, x86_movupd_from_reg,
    x86_haddps, x86_haddpd,
    x86_shufps, x86_shufpd,
    x86_cvtdq2ps, x86_cvttps2dq,
    -- ** Prefetch instructions
    x86_prefetch0_mem, x86_prefetch1_mem, x86_prefetch2_mem, x86_prefetchnta_mem,
    x86_prefetch0_membase, x86_prefetch1_membase, x86_prefetch2_membase, x86_prefetchnta_membase,
    x86_prefetch0_regp, x86_prefetch1_regp, x86_prefetch2_regp, x86_prefetchnta_regp,
    -- ** Miscellaneous
    x86_sahf, x86_wait, x86_nop, x86_breakpoint, x86_rdtsc, x86_cld,
    x86_prefix, x86_padding,
    -- ** Other utilities
    negateCC
                       ) where

import qualified Text.PrettyPrint.HughesPJ as PP

import Data.Word
import Data.Bits

import Foreign.Ptr

import Harpy.CodeGenMonad

-- | Maximal length of an x86 instruction in bytes.
x86_max_instruction_bytes :: Int
x86_max_instruction_bytes = 16   -- According to Intel manual.

x86_dword_size, x86_qword_size :: Int

x86_dword_size = 4                    -- Number of bytes in doubleword
x86_qword_size = 8                    -- Number of bytes in quadword

x86_eax, x86_ecx, x86_edx, x86_ebx, x86_esp, x86_ebp, x86_esi,
  x86_edi :: Word8
x86_eax = 0
x86_ecx = 1
x86_edx = 2
x86_ebx = 3
x86_esp = 4
x86_ebp = 5
x86_esi = 6
x86_edi = 7

x86_cmp, x86_or, x86_adc, x86_sbb, x86_and, x86_sub, x86_xor,
  x86_add :: Word8
x86_add = 0
x86_or  = 1
x86_adc = 2
x86_sbb = 3
x86_and = 4
x86_sub = 5
x86_xor = 6
x86_cmp = 7

x86_sar, x86_shld, x86_shlr, x86_rol, x86_ror, x86_rcl, x86_rcr,
  x86_shl, x86_shr :: Word8

x86_shld = 0
x86_shlr = 1
x86_rol  = 0
x86_ror  = 1
x86_rcl  = 2
x86_rcr  = 3
x86_shl  = 4
x86_shr  = 5
x86_sar  = 7

x86_fadd, x86_fmul, x86_fcom, x86_fcomp, x86_fsub, x86_fsubr :: Word8
x86_fdiv, x86_fdivr :: Word8

x86_fadd  = 0
x86_fmul  = 1
x86_fcom  = 2
x86_fcomp = 3
x86_fsub  = 4
x86_fsubr = 5
x86_fdiv  = 6
x86_fdivr = 7

x86_cc_no, x86_cc_eq, x86_cc_e, x86_cc_z, x86_cc_ne, x86_cc_nz, x86_cc_lt :: Int
x86_cc_b, x86_cc_c, x86_cc_nae, x86_cc_le, x86_cc_be, x86_cc_na :: Int
x86_cc_gt :: Int
x86_cc_a, x86_cc_nbe, x86_cc_ge, x86_cc_ae, x86_cc_nb, x86_cc_nc :: Int
x86_cc_lz, x86_cc_s, x86_cc_gez, x86_cc_ns, x86_cc_p, x86_cc_pe :: Int
x86_cc_np, x86_cc_po, x86_cc_o :: Int
x86_cc_eq  = 0
x86_cc_e   = 0
x86_cc_z   = 0
x86_cc_ne  = 1
x86_cc_nz  = 1
x86_cc_lt  = 2
x86_cc_b   = 2
x86_cc_c   = 2
x86_cc_nae = 2
x86_cc_le  = 3
x86_cc_be  = 3
x86_cc_na  = 3
x86_cc_gt  = 4
x86_cc_a   = 4
x86_cc_nbe = 4
x86_cc_ge  = 5
x86_cc_ae  = 5
x86_cc_nb  = 5
x86_cc_nc  = 5
x86_cc_lz  = 6
x86_cc_s   = 6
x86_cc_gez = 7
x86_cc_ns  = 7
x86_cc_p   = 8
x86_cc_pe  = 8
x86_cc_np  = 9
x86_cc_po  = 9
x86_cc_o   = 10
x86_cc_no  = 11

-- | FP status
x86_fp_c0, x86_fp_c1, x86_fp_c2, x86_fp_c3, x86_fp_cc_mask :: Word32
x86_fp_c0 = 0x100
x86_fp_c1 = 0x200
x86_fp_c2 = 0x400
x86_fp_c3 = 0x4000
x86_fp_cc_mask = 0x4500

-- | FP control word
x86_fpcw_invopex_mask, x86_fpcw_denopex_mask, x86_fpcw_zerodiv_mask,
 x86_fpcw_ovfex_mask, x86_fpcw_undfex_mask, x86_fpcw_precex_mask,
 x86_fpcw_precc_mask, x86_fpcw_roundc_mask :: Word32

x86_fpcw_invopex_mask = 0x1
x86_fpcw_denopex_mask = 0x2
x86_fpcw_zerodiv_mask = 0x4
x86_fpcw_ovfex_mask   = 0x8
x86_fpcw_undfex_mask  = 0x10
x86_fpcw_precex_mask  = 0x20
x86_fpcw_precc_mask   = 0x300
x86_fpcw_roundc_mask  = 0xc00

-- | Values for precision control
x86_fpcw_prec_single, x86_fpcw_prec_double,
 x86_fpcw_prec_extended :: Word32
x86_fpcw_prec_single    = 0
x86_fpcw_prec_double    = 0x200
x86_fpcw_prec_extended  = 0x300

-- | Values for rounding control
x86_fpcw_round_nearest, x86_fpcw_round_down, x86_fpcw_round_up,
 x86_fpcw_round_tozero :: Word32
x86_fpcw_round_nearest  = 0
x86_fpcw_round_down     = 0x400
x86_fpcw_round_up       = 0x800
x86_fpcw_round_tozero   = 0xc00

-- | Prefix codes
x86_lock_prefix, x86_repnz_prefix, x86_repz_prefix, x86_rep_prefix,
 x86_cs_prefix, x86_ss_prefix, x86_ds_prefix, x86_es_prefix,
 x86_fs_prefix, x86_gs_prefix, x86_unlikely_prefix,
 x86_likely_prefix, x86_operand_prefix, x86_address_prefix :: Word8
x86_lock_prefix = 0xf0
x86_repnz_prefix = 0xf2
x86_repz_prefix = 0xf3
x86_rep_prefix = 0xf3
x86_cs_prefix = 0x2e
x86_ss_prefix = 0x36
x86_ds_prefix = 0x3e
x86_es_prefix = 0x26
x86_fs_prefix = 0x64
x86_gs_prefix = 0x65
x86_unlikely_prefix = 0x2e
x86_likely_prefix = 0x3e
x86_operand_prefix = 0x66
x86_address_prefix = 0x67

-- | Mapping from condition code to opcode (unsigned)
x86_cc_unsigned_map :: [Word8]
x86_cc_unsigned_map = [
       0x74, -- eq
       0x75, -- ne
       0x72, -- lt
       0x76, -- le
       0x77, -- gt
       0x73, -- ge
       0x78, -- lz
       0x79, -- gez
       0x7a, -- p
       0x7b, -- np
       0x70, -- o
       0x71  -- no
 ]

-- | Mapping from condition code to opcode (signed)
x86_cc_signed_map :: [Word8]
x86_cc_signed_map = [
      0x74, -- eq
      0x75, -- ne
      0x7c, -- lt
      0x7e, -- le
      0x7f, -- gt
      0x7d, -- ge
      0x78, -- lz
      0x79, -- gez
      0x7a, -- p
      0x7b, -- np
      0x70, -- o
      0x71  -- no
 ]

-- | Mapping from condition code to negated condition code.
x86_cc_negate :: [(Int, Int)]
x86_cc_negate = [
       (x86_cc_eq, x86_cc_ne), -- eq
       (x86_cc_ne, x86_cc_eq), -- ne
       (x86_cc_lt, x86_cc_ge), -- lt
       (x86_cc_le, x86_cc_gt), -- le
       (x86_cc_gt, x86_cc_le), -- gt
       (x86_cc_ge, x86_cc_lt), -- ge
       (x86_cc_lz, x86_cc_gez), -- lz
       (x86_cc_gez, x86_cc_lz), -- gez
       (x86_cc_p, x86_cc_np), -- p
       (x86_cc_np, x86_cc_p), -- np
       (x86_cc_o, x86_cc_no), -- o
       (x86_cc_no, x86_cc_o)  -- no
 ]

-- | Invert a condition code.
negateCC :: Int -> Int
negateCC cc =
    case lookup cc x86_cc_negate of
      Just cc' -> cc'
      Nothing -> error ("unhandled case in negateCC" ++ show cc)

-- | Used to encode the fact that no base register is used in an
-- instruction.
x86_nobasereg :: Word8
x86_nobasereg = (-1)

x86_edi_mask, x86_esi_mask, x86_ebx_mask, x86_ebp_mask,
    x86_eax_mask, x86_ecx_mask, x86_edx_mask:: Int
x86_esi_mask = (1 `shiftL` (fromIntegral x86_esi))
x86_edi_mask = (1 `shiftL` (fromIntegral x86_edi))
x86_ebx_mask = (1 `shiftL` (fromIntegral x86_ebx))
x86_ebp_mask = (1 `shiftL` (fromIntegral x86_ebp))
x86_eax_mask = (1 `shiftL` (fromIntegral x86_eax))
x86_ecx_mask = (1 `shiftL` (fromIntegral x86_ecx))
x86_edx_mask = (1 `shiftL` (fromIntegral x86_edx))

-- | Bitvector mask for callee-saved registers
x86_callee_regs :: Int
x86_callee_regs = ((1 `shiftL` (fromIntegral x86_eax)) .|.
           (1 `shiftL` (fromIntegral x86_ecx)) .|.
           (1 `shiftL` (fromIntegral x86_edx)))

-- | Bitvector mask for caller-saved registers
x86_caller_regs :: Int
x86_caller_regs = ((1 `shiftL` (fromIntegral x86_ebx)) .|.
           (1 `shiftL` (fromIntegral x86_ebp)) .|.
           (1 `shiftL` (fromIntegral x86_esi)) .|.
           (1 `shiftL` (fromIntegral x86_edi)))

-- | Bitvector mask for byte-adressable registers
x86_byte_regs :: Int
x86_byte_regs =  ((1 `shiftL` (fromIntegral x86_eax)) .|.
          (1 `shiftL` (fromIntegral x86_ecx)) .|.
          (1 `shiftL` (fromIntegral x86_edx)) .|.
          (1 `shiftL` (fromIntegral x86_ebx)))

-- | Returns true when the given register is caller-saved.
x86_is_scratch :: Int -> Bool
x86_is_scratch reg = (x86_caller_regs .&. (1 `shiftL` (reg))) /= 0

-- | Returns true when the given register is caller-saved.
x86_is_callee :: Int -> Bool

x86_is_callee reg =  (x86_callee_regs .&. (1 `shiftL` (reg))) /= 0

-- | Returns true when the given register is byte-addressable.
x86_is_byte_reg :: (Num a, Ord a) => a -> Bool
x86_is_byte_reg reg = ((reg) < 4)



-- useful building blocks


--x86_modrm_mod modrm = ((modrm) `shiftR` 6)
--x86_modrm_reg :: Bits a => a -> a
--x86_modrm_reg modrm = (((modrm) `shiftR` 3) .&. 0x7)
--x86_modrm_rm modrm = ((modrm) .&. 0x7)

x86_address_byte :: Word8 -> Word8 -> Word8 -> CodeGen e s ()
x86_address_byte m o r = emit8 ((((m) .&. 0x03) `shiftL` 6) .|.
                               (((o) .&. 0x07) `shiftL` 3) .|.
                                (((r) .&. 0x07)))

-- | Emit a 32-bit constant to the instruction stream.
x86_imm_emit32 :: Word32 -> CodeGen e s ()
x86_imm_emit32 imm = emit32 imm

-- -- | Emit a 32-bit constant to the instruction stream at the given offset.
-- x86_imm_emit32_at :: Int -> Word32 -> CodeGen e s ()
-- x86_imm_emit32_at pos imm = emit32At pos imm

-- | Emit a 16-bit constant to the instruction stream.
x86_imm_emit16 :: Word16 -> CodeGen e s ()
x86_imm_emit16 imm =
    let b0 = (imm .&. 0xff)
        b1 = ((imm `shiftR` 8) .&. 0xff)
    in do emit8 (fromIntegral b0)
          emit8 (fromIntegral b1)

-- | Emit a 8-bit constant to the instruction stream.
x86_imm_emit8 :: Word8 -> CodeGen e s ()
x86_imm_emit8 imm =
  emit8 (imm .&. 0xff)

-- -- | Emit a 8-bit constant to the instruction stream at the given offset.
-- x86_imm_emit8_at :: Int -> Word8 -> CodeGen e s ()
-- x86_imm_emit8_at pos imm = emit8At pos (imm .&. 0xff)

-- | Return true if the given value is a signed 8-bit constant.
x86_is_imm8 :: Integral a => a -> Bool
x86_is_imm8 imm =  (((fromIntegral imm :: Integer) >= -128) && ((fromIntegral imm :: Integer) <= 127))
-- x86_is_imm16 :: Integral a => a -> Bool
-- x86_is_imm16 imm = (((fromIntegral imm :: Integer) >= -(1 `shiftL` 16)) &&
--                               ((fromIntegral imm :: Integer) <= ((1 `shiftL` 16)-1)))

x86_reg_emit :: Word8 -> Word8 -> CodeGen e s ()
x86_reg_emit r regno = x86_address_byte 3 r regno

x86_reg8_emit :: Word8 -> Word8 -> Bool -> Bool -> CodeGen e s ()
x86_reg8_emit r regno is_rh is_rnoh =
  x86_address_byte 3 (if is_rh then (r .|. 4) else r)
                     (if is_rnoh then regno .|. 4 else regno)

-- | Emit a register-indirect address encoding.
x86_regp_emit :: Word8 -> Word8 -> CodeGen e s ()
x86_regp_emit r regno = x86_address_byte 0 r regno

-- | Emit a memory+displacement address encoding.
x86_mem_emit :: Word8 -> Word32 -> CodeGen e s ()
x86_mem_emit r disp = do x86_address_byte 0 r 5
                         x86_imm_emit32 disp

-- | Emit a mem+base address encoding
x86_membase_emit :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_membase_emit r basereg disp =
    if basereg == x86_esp
       then if disp == 0
               then do x86_address_byte 0 r x86_esp
                       x86_address_byte 0 x86_esp x86_esp
               else if x86_is_imm8 disp
                       then do x86_address_byte 1 r x86_esp
                               x86_address_byte 0 x86_esp x86_esp
                               x86_imm_emit8 (fromIntegral disp)
                       else do x86_address_byte 2 r x86_esp
                               x86_address_byte 0 x86_esp x86_esp
                               x86_imm_emit32 (fromIntegral disp)
       else do if (disp == 0 && (toInteger basereg) /= (toInteger x86_ebp))
                  then x86_address_byte 0 r basereg
                  else if x86_is_imm8 (fromIntegral disp :: Word32)
                          then do x86_address_byte 1 r basereg
                                  x86_imm_emit8 (fromIntegral disp)
                          else do x86_address_byte 2 r basereg
                                  x86_imm_emit32 (fromIntegral disp)

x86_memindex_emit :: Word8 -> Word8 -> Word32 -> Word8 -> Word8 -> CodeGen e s ()
x86_memindex_emit r basereg disp indexreg shft =
    if (basereg == x86_nobasereg)
       then do x86_address_byte 0 r 4
               x86_address_byte shft indexreg 5
               x86_imm_emit32 disp
       else if ((disp) == 0 && (basereg) /= x86_ebp)
               then do x86_address_byte 0 r 4
                       x86_address_byte shft indexreg (fromIntegral basereg)
                else if x86_is_imm8 disp
                        then do x86_address_byte 1 r 4
                                x86_address_byte shft indexreg
                                             (fromIntegral basereg)
                                x86_imm_emit8 (fromIntegral disp)
                        else do x86_address_byte 2 r 4
                                x86_address_byte shft indexreg 5
                                x86_imm_emit32 disp

{-
x86_jmp_ofs_size ins =
  do instr <- peek8At ins
     case instr of
       0xe8 -> return 1
       0xe9 -> return 1
       0x0f ->
         do atPos <- peek8At (ins + 1)
            if (atPos < 0x70 || atPos > 0x8f)
               then failCodeGen (PP.text "Wrong Opcode")
               else return 1
       _ -> return 0
-}

-- target is the position in the code where to jump to:

-- target = code;
-- .. output loop code...
-- x86_mov_reg_imm (code, X86_EAX, 0);
-- loop = code;
-- x86_loop (code, -1);
-- ... finish method

-- patch displacement

-- x86_patch (loop, target);

-- ins should point at the start of the instruction that encodes a target.
-- the instruction is inspected for validity and the correct displacement
-- is inserted.

{-
x86_patch ins target =
    let pos = ins + 1
    in do size <- x86_jmp_ofs_size ins
          instr <- peek8At ins
          let disp = target - (if instr == 0x0f then pos + 1 else pos)
          if size == 1
             then x86_imm_emit32_at pos (fromIntegral (disp - 4))
             else if (x86_is_imm8 (disp - 1))
                     then x86_imm_emit8_at pos (fromIntegral (disp - 1))
                     else failCodeGen (PP.text "Wrong offset")
-}

x86_breakpoint, x86_cld, x86_stosb, x86_stosl, x86_stosd, x86_movsb,
 x86_movsl, x86_movsd :: CodeGen s e ()
x86_breakpoint = emit8 0xcc
x86_cld = emit8 0xfc
x86_stosb = emit8 0xaa
x86_stosl = emit8 0xab
x86_stosd = x86_stosl
x86_movsb = emit8 0xa4
x86_movsl = emit8 0xa5
x86_movsd = x86_movsl

x86_prefix :: Word8 -> CodeGen s e ()
x86_prefix p = emit8 p

x86_rdtsc :: CodeGen s e ()
x86_rdtsc = emit8 0x0f >> emit8 0x31

x86_cmpxchg_reg_reg :: Word8 -> Word8 -> CodeGen e s ()
x86_cmpxchg_reg_reg dreg reg =
    emit8 0x0f >> emit8 0xb1 >> x86_reg_emit reg dreg

x86_cmpxchg_mem_reg :: Word32 -> Word8 -> CodeGen e s ()
x86_cmpxchg_mem_reg mem reg = emit8 0x0f >> emit8 0xb1 >> x86_mem_emit reg mem

x86_cmpxchg_membase_reg :: Word8 -> Word32 -> Word8 -> CodeGen e s ()
x86_cmpxchg_membase_reg basereg disp reg =
    emit8 0x0f >> emit8 0xb1 >> x86_membase_emit reg basereg disp

x86_xchg :: (Eq a, Num a) => a -> CodeGen e s ()
x86_xchg size = if size == 1 then emit8 0x86 else emit8 0x87

x86_xchg_reg_reg dreg reg size =
    do x86_xchg size ; x86_reg_emit reg dreg
x86_xchg_mem_reg mem reg size =
    do x86_xchg size ; x86_mem_emit reg mem
x86_xchg_membase_reg basereg disp reg size =
    do x86_xchg size ; x86_membase_emit reg basereg disp

x86_xadd :: (Eq a, Num a) => a -> CodeGen e s ()
x86_xadd size = do emit8 0x0f ; if size == 1 then emit8 0xc0 else emit8 0xc1
x86_xadd_reg_reg dreg reg size = x86_xadd size >> x86_reg_emit reg dreg
x86_xadd_mem_reg mem reg size = x86_xadd size >> x86_mem_emit reg mem
x86_xadd_membase_reg basereg disp reg size =
    x86_xadd size >> x86_membase_emit reg basereg disp

x86_inc_mem mem = emit8 0xff >> x86_mem_emit 0 mem
x86_inc_membase basereg disp = emit8 0xff >> x86_membase_emit 0 basereg disp
x86_inc_reg reg = emit8 (0x40 + reg)

x86_dec_mem mem = emit8 0xff >> x86_mem_emit 1 mem
x86_dec_membase basereg disp = emit8 0xff >> x86_membase_emit 1 basereg disp
x86_dec_reg reg = emit8 (0x48 + reg)

x86_not_mem mem = emit8 0xf7 >> x86_mem_emit 2 mem
x86_not_membase basereg disp = emit8 0xf7 >> x86_membase_emit 2 basereg disp
x86_not_reg reg = emit8 0xf7 >> x86_reg_emit 2 reg

x86_neg_mem mem = emit8 0xf7 >> x86_mem_emit 3 mem
x86_neg_membase basereg disp = emit8 0xf7 >> x86_membase_emit 3 basereg disp
x86_neg_reg reg = emit8 0xf7 >> x86_reg_emit 3 reg

x86_nop :: CodeGen s e ()
x86_nop = emit8 0x90

x86_alu_reg_imm :: Word8 -> Word8 -> Int -> CodeGen e s ()
x86_alu_reg_imm opc reg imm =
    do if reg == x86_eax
          then emit8 (fromIntegral (((opc) `shiftL` 3) + 5)) >> x86_imm_emit32 (fromIntegral imm)
          else if x86_is_imm8 imm
                  then do emit8 0x83
                          x86_reg_emit (fromIntegral opc) (fromIntegral reg)
                          x86_imm_emit8 (fromIntegral imm)
                  else do emit8 0x81
                          x86_reg_emit (fromIntegral opc) (fromIntegral reg)
                          x86_imm_emit32 (fromIntegral imm)


x86_alu_mem_imm opc mem imm =
    if x86_is_imm8 imm
       then do emit8 0x83
               x86_mem_emit opc mem
               x86_imm_emit8 (fromIntegral imm)
       else do emit8 0x81
               x86_mem_emit opc mem
               x86_imm_emit32 imm


x86_alu_membase_imm opc basereg disp imm =
    if x86_is_imm8 imm
       then do emit8 0x83
               x86_membase_emit opc basereg disp
               x86_imm_emit8 (fromIntegral imm)
       else do emit8 0x81
               x86_membase_emit opc basereg disp
               x86_imm_emit32 imm
x86_alu_membase8_imm opc basereg disp imm =
    do emit8 0x80
       x86_membase_emit opc basereg disp
       x86_imm_emit8 imm
x86_alu_mem_reg opc mem reg =
        do emit8 ((opc `shiftL` 3) + 1)
           x86_mem_emit reg mem
x86_alu_membase_reg opc basereg disp reg =
    do emit8 ((opc `shiftL` 3) + 1)
       x86_membase_emit reg basereg disp
x86_alu_reg_reg opc dreg reg =
    do emit8 ((opc `shiftL` 3) + 3)
       x86_reg_emit dreg reg

-- @x86_alu_reg8_reg8:
-- Supports ALU operations between two 8-bit registers.
-- dreg := dreg opc reg
-- X86_Reg_No enum is used to specify the registers.
-- Additionally is_*_h flags are used to specify what part
-- of a given 32-bit register is used - high (TRUE) or low (FALSE).
-- For example: dreg = X86_EAX, is_dreg_h = TRUE -> use AH

x86_alu_reg8_reg8 opc dreg reg is_dreg_h is_reg_h =
    do emit8 ((opc `shiftL` 3) + 2)
       x86_reg8_emit dreg reg is_dreg_h is_reg_h
x86_alu_reg_mem opc reg mem =
    do emit8 ((opc `shiftL` 3) + 3)
       x86_mem_emit reg mem
x86_alu_reg_membase opc reg basereg disp =
    do emit8 ((opc `shiftL` 3) + 3)
       x86_membase_emit reg basereg disp

x86_test_reg_imm reg imm =
    do if reg == x86_eax
          then emit8 0xa9
          else do emit8 0xf7 ; x86_reg_emit 0 (fromIntegral reg)
       x86_imm_emit32 imm
x86_test_mem_imm mem imm =
    do emit8 0xf7 ; x86_mem_emit 0 mem ; x86_imm_emit32 imm
x86_test_membase_imm basereg disp imm =
    do emit8 0xf7 ; x86_membase_emit 0 basereg disp ; x86_imm_emit32 imm
x86_test_reg_reg dreg reg = do emit8 0x85 ; x86_reg_emit reg dreg
x86_test_mem_reg mem reg =
    do emit8 0x85 ; x86_mem_emit reg mem
x86_test_membase_reg basereg disp reg =
    do emit8 0x85 ; x86_membase_emit reg basereg disp

x86_shift_reg_imm opc reg imm =
    if imm == 1
       then do emit8 0xd1 ; x86_reg_emit opc reg
       else do emit8 0xc1
               x86_reg_emit opc reg
               x86_imm_emit8 imm
x86_shift_mem_imm opc mem imm =
    if imm == 1
       then do emit8 0xd1 ; x86_mem_emit opc mem
       else do emit8 0xc1
               x86_mem_emit opc mem
               x86_imm_emit8 imm
x86_shift_membase_imm opc basereg disp imm =
    if imm == 1
       then do emit8 0xd1 ; x86_membase_emit opc basereg disp
       else do emit8 0xc1
               x86_membase_emit opc basereg disp
               x86_imm_emit8 imm
x86_shift_reg opc reg =
    emit8 0xd3 >> x86_reg_emit opc reg
x86_shift_mem opc mem =
    emit8 0xd3 >> x86_mem_emit opc mem
x86_shift_membase opc basereg disp =
    emit8 0xd3 >> x86_membase_emit opc basereg disp

-- Multi op shift missing.

x86_shrd_reg dreg reg =
    emit8 0x0f >> emit8 0xad >> x86_reg_emit reg dreg
x86_shrd_reg_imm dreg reg shamt =
    emit8 0x0f >> emit8 0xac >> x86_reg_emit reg dreg >> x86_imm_emit8 shamt
x86_shld_reg dreg reg =
    emit8 0x0f >> emit8 0xa5 >> x86_reg_emit reg dreg
x86_shld_reg_imm dreg reg shamt =
    emit8 0x0f >> emit8 0xa4 >> x86_reg_emit reg dreg >>x86_imm_emit8 shamt

-- EDX:EAX = EAX * rm

x86_mul_reg :: Word8 -> Bool -> CodeGen e s ()
x86_mul_reg reg is_signed =
    emit8 0xf7 >> x86_reg_emit (4 + (if is_signed then 1 else 0)) reg

x86_mul_mem :: Word32 -> Bool -> CodeGen e s ()
x86_mul_mem mem is_signed =
    emit8 0xf7 >> x86_mem_emit (4 + (if is_signed then 1 else 0)) mem

x86_mul_membase :: Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_mul_membase basereg disp is_signed =
    do emit8 0xf7
       x86_membase_emit (4 + (if is_signed then 1 else 0)) basereg disp

-- r *= rm

x86_imul_reg_reg :: Word8 -> Word8 -> CodeGen e s ()
x86_imul_reg_reg dreg reg =
    emit8 0x0f >> emit8 0xaf >> x86_reg_emit dreg reg

x86_imul_reg_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_imul_reg_mem reg mem =
    emit8 0x0f >> emit8 0xaf >> x86_mem_emit reg mem

x86_imul_reg_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_imul_reg_membase reg basereg disp =
    emit8 0x0f >> emit8 0xaf >> x86_membase_emit reg basereg disp

-- dreg = rm * imm

x86_imul_reg_reg_imm :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_imul_reg_reg_imm dreg reg imm =
    if x86_is_imm8 imm
       then emit8 0x6b >> x86_reg_emit dreg reg >>
              x86_imm_emit8 (fromIntegral imm)
       else emit8 0x69 >> x86_reg_emit dreg reg >> x86_imm_emit32 imm

x86_imul_reg_mem_imm :: Word8 -> Word32 -> Word32 -> CodeGen e s ()
x86_imul_reg_mem_imm reg mem imm =
    if x86_is_imm8 imm
       then emit8 0x6b >> x86_mem_emit reg mem >>
              x86_imm_emit8 (fromIntegral imm)
       else emit8 0x69 >> x86_reg_emit reg (fromIntegral mem) >>
              x86_imm_emit32 imm

x86_imul_reg_membase_imm :: Word8 -> Word8 -> Word32 -> Word32 -> CodeGen e s ()
x86_imul_reg_membase_imm reg basereg disp imm =
    if x86_is_imm8 imm
       then emit8 0x6b >> x86_membase_emit reg basereg disp >>
            x86_imm_emit8 (fromIntegral imm)
       else do emit8 0x69
               x86_membase_emit reg basereg disp
               x86_imm_emit32 imm

-- divide EDX:EAX by rm;
-- eax = quotient, edx = remainder

x86_div_reg :: Word8 -> Bool -> CodeGen e s ()
x86_div_reg reg is_signed =
    emit8 0xf7 >> x86_reg_emit (6 + (if is_signed then 1 else 0)) reg
x86_div_mem :: Word32 -> Bool -> CodeGen e s ()
x86_div_mem mem is_signed =
    emit8 0xf7 >> x86_mem_emit (6 + (if is_signed then 1 else 0)) mem

x86_div_membase :: Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_div_membase basereg disp is_signed =
    do emit8 0xf7
       x86_membase_emit (6 + (if is_signed then 1 else 0)) basereg disp

x86_mov1 :: (Eq t, Num t) => t -> CodeGen e s ()
x86_mov1 size =
    case size of
         1 -> emit8 0x88
         2 -> emit8 0x66 >> emit8 0x89
         4 -> emit8 0x89
         _ -> failCodeGen (PP.text "invalid operand size")

x86_mov2 :: (Eq t, Num t) => t -> CodeGen e s ()
x86_mov2 size =
    case size of
         1 -> emit8 0x8a
         2 -> emit8 0x66 >> emit8 0x8b
         4 -> emit8 0x8b
         _ -> failCodeGen (PP.text "invalid operand size")

x86_mov_mem_reg :: (Eq t, Num t) => Word32 -> Word8 -> t -> CodeGen e s ()
x86_mov_mem_reg mem reg size =
    do x86_mov1 size ; x86_mem_emit reg mem

x86_mov_regp_reg :: (Eq t, Num t) => Word8 -> Word8 -> t -> CodeGen e s ()
x86_mov_regp_reg regp reg size =
    do x86_mov1 size ; x86_regp_emit reg regp

x86_mov_reg_regp :: (Eq t, Num t) => Word8 -> Word8 -> t -> CodeGen e s ()
x86_mov_reg_regp reg regp size =
    do x86_mov2 size ; x86_regp_emit reg regp

x86_mov_membase_reg :: (Eq t, Num t) => Word8 -> Word32 -> Word8 -> t -> CodeGen e s ()
x86_mov_membase_reg basereg disp reg size =
    do x86_mov1 size ; x86_membase_emit reg basereg disp

x86_mov_memindex_reg :: (Eq t, Num t) => Word8 -> Word32 -> Word8 -> Word8 -> Word8 -> t -> CodeGen e s ()
x86_mov_memindex_reg basereg disp indexreg shft reg size =
    do x86_mov1 size ; x86_memindex_emit reg basereg disp indexreg shft

x86_mov_reg_reg :: (Eq t, Num t) => Word8 -> Word8 -> t -> CodeGen e s ()
x86_mov_reg_reg dreg reg size =
    do x86_mov2 size
       x86_reg_emit dreg reg

x86_mov_reg_mem :: (Eq t, Num t) => Word8 -> Word32 -> t -> CodeGen e s ()
x86_mov_reg_mem reg mem size =
    do x86_mov2 size
       x86_mem_emit reg mem

x86_mov_reg_membase :: (Eq t, Num t) => Word8 -> Word8 -> Word32 -> t -> CodeGen e s ()
x86_mov_reg_membase reg basereg disp size =
    do x86_mov2 size
       x86_membase_emit reg basereg disp

x86_mov_reg_memindex :: (Eq t, Num t) => Word8 -> Word8 -> Word32 -> Word8 -> Word8 -> t -> CodeGen e s ()
x86_mov_reg_memindex _ _ _ 4 _ _ =
    failCodeGen $ PP.text "x86_mov_reg_memindex: cannot use (E)SP as index register"
x86_mov_reg_memindex reg basereg disp indexreg shft size =
    do x86_mov2 size
       x86_memindex_emit reg basereg disp indexreg  shft

x86_mov_reg_imm :: Word8 -> Word32 -> CodeGen e s ()
x86_mov_reg_imm reg imm =
    emit8 (0xb8 + reg) >> x86_imm_emit32 imm

x86_mov_mem_imm :: (Eq a, Num a) => Word32 -> Word32 -> a -> CodeGen e s ()
x86_mov_mem_imm mem imm size =
    if size == 1
       then do emit8 0xc6;
               x86_mem_emit 0 mem
               x86_imm_emit8 (fromIntegral imm)
       else if size == 2
               then do emit8 0x66
                       emit8 0xc7
                       x86_mem_emit 0 mem
                       x86_imm_emit16 (fromIntegral imm)
               else do emit8 0xc7
                       x86_mem_emit 0 mem
                       x86_imm_emit32 imm

x86_mov_membase_imm :: (Eq a, Num a) => Word8 -> Word32 -> Word32 -> a -> CodeGen e s ()
x86_mov_membase_imm basereg disp imm size =
    if size == 1
       then do emit8 0xc6
               x86_membase_emit 0 basereg disp
               x86_imm_emit8 (fromIntegral imm)
       else if size == 2
               then do emit8 0x66
                       emit8 0xc7
                       x86_membase_emit 0 basereg disp
                       x86_imm_emit16 (fromIntegral imm)
               else do emit8 0xc7
                       x86_membase_emit 0 basereg disp
                       x86_imm_emit32 imm

x86_mov_memindex_imm :: (Eq a, Num a) => Word8 -> Word32 -> Word8 -> Word8 -> Word32 -> a -> CodeGen e s ()
x86_mov_memindex_imm basereg disp indexreg shft imm size =
    if size == 1
    then do emit8 0xc6
            x86_memindex_emit 0 basereg disp indexreg  shft
            x86_imm_emit8 (fromIntegral imm)
    else if size == 2
         then do emit8 0x66
                 emit8 0xc7
                 x86_memindex_emit 0 basereg disp indexreg  shft
                 x86_imm_emit16 (fromIntegral imm)
         else do emit8 0xc7
                 x86_memindex_emit 0 basereg disp indexreg  shft
                 x86_imm_emit32 imm

-- LEA: Load Effective Address

x86_lea_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_lea_mem reg mem = emit8 0x8d >> x86_mem_emit reg mem

x86_lea_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_lea_membase reg basereg disp =
    emit8 0x8d >> x86_membase_emit reg basereg disp

x86_lea_memindex :: Word8 -> Word8 -> Word32 -> Word8 -> Word8 -> CodeGen e s ()
x86_lea_memindex reg basereg disp indexreg shft =
    emit8 0x8d >> x86_memindex_emit reg basereg disp indexreg shft

x86_widen_reg :: Word8 -> Word8 -> Bool -> Bool -> CodeGen e s ()
x86_widen_reg dreg reg is_signed is_half =
    if is_half || x86_is_byte_reg reg
    then do let op = 0xb6 + (if is_signed then 0x08 else 0) +
                     (if is_half then 0x1 else 0)
            emit8 0x0f
            emit8 op
            x86_reg_emit dreg reg
    else failCodeGen (PP.text "widen: need byte register or is_half=True")

x86_widen_mem :: Word8 -> Word32 -> Bool -> Bool -> CodeGen e s ()
x86_widen_mem dreg mem is_signed is_half =
    do let op = 0xb6 + (if is_signed then 0x08 else 0) +
                (if is_half then 0x1 else 0)
       emit8 0x0f
       emit8 op
       x86_mem_emit dreg mem

x86_widen_membase :: Word8 -> Word8 -> Word32 -> Bool -> Bool -> CodeGen e s ()
x86_widen_membase dreg basereg disp is_signed is_half =
    do let op = 0xb6 + (if is_signed then 0x08 else 0) +
                (if is_half then 0x1 else 0)
       emit8 0x0f
       emit8 op
       x86_membase_emit dreg basereg disp

x86_widen_memindex :: Word8 -> Word8 -> Word32 -> Word8 -> Word8 -> Bool -> Bool -> CodeGen e s ()
x86_widen_memindex dreg basereg disp indexreg shft is_signed is_half =
    do let op = 0xb6 + (if is_signed then 0x08 else 0) +
                (if is_half then 0x1 else 0)
       emit8 0x0f
       emit8 op
       x86_memindex_emit dreg basereg disp indexreg shft

x86_cdq, x86_wait :: CodeGen s e ()
x86_cdq  = emit8 0x99
x86_wait = emit8 0x9b

x86_fp_op_mem :: Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_fp_op_mem opc mem is_double =
    do emit8 (if is_double then 0xdc else 0xd8)
       x86_mem_emit opc mem
x86_fp_op_membase :: Word8 -> Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_fp_op_membase opc basereg disp is_double =
    do emit8 (if is_double then 0xdc else 0xd8)
       x86_membase_emit opc basereg disp
x86_fp_op ::Word8 -> Word8 -> CodeGen e s ()
x86_fp_op opc index =
    do emit8 0xd8
       emit8 (0xc0 + (opc `shiftL` 3) + (index .&. 0x07))
x86_fp_op_reg :: Word8 -> Word8 -> Bool -> CodeGen e s ()
x86_fp_op_reg opc index pop_stack =
    do let  opcMap = [ 0, 1, 2, 3, 5, 4, 7, 6, 8]
       emit8 (if pop_stack then 0xde else 0xdc)
       emit8 (0xc0 + ((opcMap !! fromIntegral opc) `shiftL` 3) + (index .&. 0x07))


-- @x86_fp_int_op_membase
-- Supports FPU operations between ST(0) and integer operand in memory.
-- Operation encoded using X86_FP_Opcode enum.
-- Operand is addressed by [basereg + disp].
-- is_int specifies whether operand is int32 (TRUE) or int16 (FALSE).

x86_fp_int_op_membase :: Word8 -> Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_fp_int_op_membase opc basereg disp is_int =
    do emit8 (if is_int then 0xda else 0xde)
       x86_membase_emit opc basereg disp
x86_fstp :: Word8 -> CodeGen e s ()
x86_fstp index =
    emit8 0xdd >> emit8 (0xd8 + index)
x86_fcompp :: CodeGen e s ()
x86_fcompp = emit8 0xde >> emit8 0xd9
x86_fucompp :: CodeGen e s ()
x86_fucompp = emit8 0xda >> emit8 0xe9
x86_fnstsw :: CodeGen e s ()
x86_fnstsw = emit8 0xdf >> emit8 0xe0
x86_fnstcw :: Word32 -> CodeGen e s ()
x86_fnstcw mem = emit8 0xd9 >> x86_mem_emit 7 mem
x86_fnstcw_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_fnstcw_membase basereg disp =
    emit8 0xd9 >> x86_membase_emit 7 basereg disp
x86_fldcw :: Word32 -> CodeGen e s ()
x86_fldcw mem = emit8 0xd9 >> x86_mem_emit 5 mem
x86_fldcw_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_fldcw_membase basereg disp =
    emit8 0xd9 >> x86_membase_emit 5 basereg disp
x86_fchs :: CodeGen e s ()
x86_fchs = emit8 0xd9 >> emit8 0xe0
x86_frem :: CodeGen e s ()
x86_frem = emit8 0xd9 >> emit8 0xf8
x86_fxch :: Word8 -> CodeGen e s ()
x86_fxch index = emit8 0xd9 >> emit8 (0xc8 + (index .&. 0x07))
x86_fcomi :: Word8 -> CodeGen e s ()
x86_fcomi index = emit8 0xdb >> emit8 (0xf0 + (index .&. 0x07))
x86_fcomip :: Word8 -> CodeGen e s ()
x86_fcomip index = emit8 0xdf >> emit8 (0xf0 + (index .&. 0x07))
x86_fucomi :: Word8 -> CodeGen e s ()
x86_fucomi index = emit8 0xdb >> emit8 (0xe8 + (index .&. 0x07))
x86_fucomip :: Word8 -> CodeGen e s ()
x86_fucomip index = emit8 0xdf >> emit8 (0xe8 + (index .&. 0x07))

data FIntSize = FInt16 | FInt32 | FInt64

x86_fld :: Word32 -> Bool -> CodeGen e s ()
x86_fld mem is_double =
    do emit8 (if is_double then 0xdd else 0xd9)
       x86_mem_emit 0 mem
x86_fld_membase :: Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_fld_membase basereg disp is_double =
    do emit8 (if is_double then 0xdd else 0xd9)
       x86_membase_emit 0 basereg disp
x86_fld80_mem :: Word32 -> CodeGen e s ()
x86_fld80_mem mem = emit8 0xdb >> x86_mem_emit 5 mem
x86_fld80_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_fld80_membase basereg disp =
    emit8 0xdb >> x86_membase_emit 5 basereg disp
x86_fild :: Word32 -> FIntSize -> CodeGen e s ()
x86_fild mem size =
    case size of
       FInt16 -> emit8 0xdf >> x86_mem_emit 0 mem
       FInt32 -> emit8 0xdb >> x86_mem_emit 0 mem
       FInt64 -> emit8 0xdf >> x86_mem_emit 5 mem
x86_fild_membase :: Word8 -> Word32 -> FIntSize -> CodeGen e s ()
x86_fild_membase basereg disp size =
    case size of
       FInt16 -> emit8 0xdb >> x86_membase_emit 0 basereg disp
       FInt32 -> emit8 0xdb >> x86_membase_emit 0 basereg disp
       FInt64 -> emit8 0xdf >> x86_membase_emit 5 basereg disp
x86_fld_reg :: Word8 -> CodeGen e s ()
x86_fld_reg index =
    emit8 0xd9 >> emit8 (0xc0 + (index .&. 0x07))
x86_fldz :: CodeGen e s ()
x86_fldz = emit8 0xd9 >> emit8 0xee
x86_fld1 :: CodeGen e s ()
x86_fld1 = emit8 0xd9 >> emit8 0xe8
x86_fldpi :: CodeGen e s ()
x86_fldpi = emit8 0xd9 >> emit8 0xeb

x86_fst :: Word32 -> Bool -> Bool -> CodeGen e s ()
x86_fst mem is_double pop_stack =
    do emit8 (if is_double then 0xdd else 0xd9)
       x86_mem_emit (2 + (if pop_stack then 1 else 0)) mem
x86_fst_membase :: Word8 -> Word32 -> Bool -> Bool -> CodeGen e s ()
x86_fst_membase basereg disp is_double pop_stack =
    do emit8 (if is_double then 0xdd else 0xd9)
       x86_membase_emit (2 + (if pop_stack then 1 else 0)) basereg disp
x86_fst80_mem :: Word32 -> CodeGen e s ()
x86_fst80_mem mem = emit8 0xdb >> x86_mem_emit 7 mem
x86_fst80_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_fst80_membase basereg disp =
    emit8 0xdb >> x86_membase_emit 7 basereg disp
x86_fist_pop :: Word32 -> FIntSize -> CodeGen e s ()
x86_fist_pop mem size =
    case size of
       FInt16 -> emit8 0xdf >> x86_mem_emit 3 mem
       FInt32 -> emit8 0xdb >> x86_mem_emit 3 mem
       FInt64 -> emit8 0xdf >> x86_mem_emit 7 mem
x86_fist_pop_membase :: Word8 -> Word32 -> FIntSize -> CodeGen e s ()
x86_fist_pop_membase basereg disp size =
    case size of
       FInt16 -> emit8 0xdf >> x86_membase_emit 3 basereg disp
       FInt32 -> emit8 0xdb >> x86_membase_emit 3 basereg disp
       FInt64 -> emit8 0xdf >> x86_membase_emit 7 basereg disp
x86_fstsw :: CodeGen e s ()
x86_fstsw = emit8 0x9b >> emit8 0xdf >> emit8 0xe0

-- @x86_fist_membase
-- Converts content of ST(0) to integer and stores it at memory location
-- addressed by [basereg + disp].
-- size specifies whether destination is int32 or int16.

x86_fist_membase :: Word8 -> Word32 -> FIntSize -> CodeGen e s ()
x86_fist_membase basereg disp size =
    case size of
       FInt16 -> emit8 0xdf >> x86_membase_emit 2 basereg disp
       FInt32 -> emit8 0xdb >> x86_membase_emit 2 basereg disp
       FInt64 -> error "fist does not support 64 bit access"

x86_fincstp :: CodeGen e s ()
x86_fincstp = emit8 0xd9 >> emit8 0xf7

x86_fdecstp :: CodeGen e s ()
x86_fdecstp = emit8 0xd9 >> emit8 0xf6

-- PUSH instruction.

x86_push_reg :: Word8 -> CodeGen e s ()
x86_push_reg reg = emit8 (0x50 + reg)

x86_push_regp :: Word8 -> CodeGen e s ()
x86_push_regp reg = emit8 0xff >> x86_regp_emit 6 reg

x86_push_mem :: Word32 -> CodeGen e s ()
x86_push_mem mem = emit8 0xff >> x86_mem_emit 6 mem

x86_push_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_push_membase basereg disp =
    emit8 0xff >> x86_membase_emit 6 basereg disp

x86_push_memindex :: Word8 -> Word32 -> Word8 -> Word8 -> CodeGen e s ()
x86_push_memindex basereg disp indexreg shft =
    emit8 0xff >> x86_memindex_emit 6 basereg disp indexreg shft

x86_push_imm_template :: CodeGen e s ()
x86_push_imm_template = x86_push_imm 0xf0f0f0f0

x86_push_imm :: Word32 -> CodeGen e s ()
x86_push_imm imm =
    if x86_is_imm8 imm
    then emit8 0x6A >> x86_imm_emit8 (fromIntegral imm)
    else emit8 0x68 >> x86_imm_emit32 imm

-- POP instruction.

x86_pop_reg :: Word8 -> CodeGen e s ()
x86_pop_reg reg = emit8 (0x58 + reg)

x86_pop_mem :: Word32 -> CodeGen e s ()
x86_pop_mem mem = emit8 0x87 >> x86_mem_emit 0 mem

x86_pop_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_pop_membase basereg disp =
    emit8 0x87 >> x86_membase_emit 0 basereg disp

x86_pushad :: CodeGen e s ()
x86_pushad = emit8 0x60

x86_pushfd :: CodeGen e s ()
x86_pushfd = emit8 0x9c

x86_popad :: CodeGen e s ()
x86_popad  = emit8 0x61

x86_popfd :: CodeGen e s ()
x86_popfd  = emit8 0x9d

x86_loop ::  Word8 -> CodeGen e s ()
x86_loop imm = emit8 0xe2 >> x86_imm_emit8 imm

x86_loope :: Word8 -> CodeGen e s ()
x86_loope imm = emit8 0xe1 >> x86_imm_emit8 imm

x86_loopne :: Word8 -> CodeGen e s ()
x86_loopne imm = emit8 0xe0 >> x86_imm_emit8 imm

x86_jump32 :: Word32 -> CodeGen e s ()
x86_jump32 imm = emit8 0xe9 >> x86_imm_emit32 imm

x86_jump8 :: Word8 -> CodeGen e s ()
x86_jump8 imm = emit8 0xeb >> x86_imm_emit8 imm

x86_jump_reg :: Word8 -> CodeGen e s ()
x86_jump_reg reg = emit8 0xff >> x86_reg_emit 4 reg

x86_jump_mem :: Word32 -> CodeGen e s ()
x86_jump_mem mem = emit8 0xff >> x86_mem_emit 4 mem

x86_jump_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_jump_membase basereg disp =
    emit8 0xff >> x86_membase_emit 4 basereg disp

x86_jump_pointer :: Ptr a -> CodeGen e s ()
x86_jump_pointer target =
    do inst <- getCodeOffset
       base <- getBasePtr
       let ptr = base `plusPtr` inst
       x86_jump32 (fromIntegral (target `minusPtr` ptr - 5))

-- target is a pointer in our buffer.

{-
x86_jump_code target =
    do inst <- getCodeOffset
       let t = target - inst - 2
       if x86_is_imm8 t
          then x86_jump8 (fromIntegral t)
          else x86_jump32 (fromIntegral (t - 3))
-}
{-
x86_jump_disp disp =
    do let t = disp - 2
       if x86_is_imm8 t
          then x86_jump8 (fromIntegral t)
          else x86_jump32 (t - 3)
-}

x86_branch8 :: Int -> Word8 -> Bool -> CodeGen e s ()
x86_branch8 cond imm is_signed =
    do if is_signed
          then emit8 (x86_cc_signed_map !! cond)
          else emit8 (x86_cc_unsigned_map !! cond)
       x86_imm_emit8 imm

x86_branch32 :: Int -> Word32 -> Bool -> CodeGen e s ()
x86_branch32 cond imm is_signed =
    do emit8 0x0f
       if is_signed
          then emit8 ((x86_cc_signed_map !! cond) + 0x10)
          else emit8 ((x86_cc_unsigned_map !! cond) + 0x10)
       x86_imm_emit32 imm

x86_branch :: Int -> Int -> Bool -> CodeGen e s ()
x86_branch cond target is_signed =
    do inst <- getCodeOffset
       let offset = target - inst - 2;
       if x86_is_imm8 offset
          then x86_branch8 cond (fromIntegral offset) is_signed
          else x86_branch32 cond (fromIntegral (offset - 4)) is_signed

x86_branch_pointer :: Int -> Ptr a -> Bool -> CodeGen e s ()
x86_branch_pointer cond target is_signed =
    do inst <- getCodeOffset
       base <- getBasePtr
       let ptr = base `plusPtr` inst
       x86_branch32 cond (fromIntegral (target `minusPtr` ptr - 5)) is_signed

{-
x86_branch_disp cond disp is_signed =
    do let offset = disp - 2
       if x86_is_imm8 offset
          then x86_branch8 cond (fromIntegral offset) is_signed
          else x86_branch32 cond (offset - 4) is_signed
-}

x86_jecxz :: Word8 -> CodeGen e s ()
x86_jecxz imm = emit8 0xe3 >> emit8 imm

x86_set_reg :: Int -> Word8 -> Bool -> CodeGen e s ()
x86_set_reg cond reg is_signed =
    do emit8 0x0f
       if is_signed
          then emit8 ((x86_cc_signed_map !! cond) + 0x20)
          else emit8 ((x86_cc_unsigned_map !! cond) + 0x20)
       x86_reg_emit 0 reg

x86_set_mem :: Int -> Word32 -> Bool -> CodeGen e s ()
x86_set_mem cond mem is_signed =
    do emit8 0x0f
       if is_signed
          then emit8 ((x86_cc_signed_map !! cond) + 0x20)
          else emit8 ((x86_cc_unsigned_map !! cond) + 0x20)
       x86_mem_emit 0 mem
x86_set_membase :: Int -> Word8 -> Word32 -> Bool -> CodeGen e s ()
x86_set_membase cond basereg disp is_signed =
    do emit8 0x0f
       if is_signed
          then emit8 ((x86_cc_signed_map !! cond) + 0x20)
          else emit8 ((x86_cc_unsigned_map !! cond) + 0x20)
       x86_membase_emit 0 basereg disp

-- Call instructions.

x86_call_imm :: Word32 -> CodeGen s e ()
x86_call_imm disp = emit8 0xe8 >> x86_imm_emit32 disp

x86_call_reg :: Word8 -> CodeGen s e ()
x86_call_reg reg = emit8 0xff >> x86_reg_emit 2 reg

x86_call_mem :: Word32 -> CodeGen s e ()
x86_call_mem mem = emit8 0xff >> x86_mem_emit 2 mem

x86_call_membase :: Word8 -> Word32 -> CodeGen s e ()
x86_call_membase basereg disp =
    emit8 0xff >> x86_membase_emit 2 basereg disp

x86_call_code :: Int -> CodeGen s e ()
x86_call_code target =
    do inst <- getCodeOffset
       let  _x86_offset = (target - inst - 5)
       x86_call_imm (fromIntegral _x86_offset)

x86_call_hs :: FunPtr a -> CodeGen e s ()
x86_call_hs fptr = do { offset <- getCodeOffset
                      ; base <- getBasePtr
                      ; emitRelocInfo (offset + 1)
                          RelocPCRel fptr
                      ; x86_call_imm $ (fromIntegral (minusPtr (castFunPtrToPtr fptr) (plusPtr base offset) - 5))
                      }

-- RET instruction.

x86_ret :: CodeGen s e ()
x86_ret = emit8 0xc3

x86_ret_imm :: Word16 -> CodeGen s e ()
x86_ret_imm imm =
    if imm == 0 then x86_ret else emit8 0xc2 >> x86_imm_emit16 imm

-- Conditional move instructions.
x86_cmov ::Int -> Bool -> CodeGen e s ()
x86_cmov cond is_signed =
    do emit8 0x0f
       if is_signed
          then emit8 ((x86_cc_signed_map !! cond) - 0x30)
          else emit8 ((x86_cc_unsigned_map !! cond) - 0x30)
x86_cmov_reg :: Int -> Bool -> Word8 -> Word8 -> CodeGen e s ()
x86_cmov_reg cond is_signed dreg reg =
    do x86_cmov cond is_signed
       x86_reg_emit dreg reg
x86_cmov_mem :: Int -> Bool -> Word8 -> Word32 -> CodeGen e s ()
x86_cmov_mem cond is_signed reg mem =
    do x86_cmov cond is_signed
       x86_mem_emit reg mem
x86_cmov_membase :: Int -> Bool -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_cmov_membase cond is_signed reg basereg disp =
    do x86_cmov cond is_signed
       x86_membase_emit reg basereg disp

-- Note: definition for ENTER instruction is not complete.  The counter
-- for the display setup is set to 0.

x86_enter :: Word16 -> CodeGen s e ()
x86_enter framesize = emit8 0xc8 >> x86_imm_emit16 framesize >> emit8 0

x86_leave :: CodeGen s e ()
x86_leave = emit8 0xc9

x86_sahf :: CodeGen s e ()
x86_sahf  = emit8 0x9e

-- Trigonometric floating point functions

x86_fsin, x86_fcos, x86_fabs, x86_ftst, x86_fxam, x86_fpatan,
 x86_fprem, x86_fprem1, x86_frndint, x86_fsqrt, x86_fptan :: CodeGen s e ()
x86_fsin    = emit8 0xd9 >> emit8 0xfe
x86_fcos    = emit8 0xd9 >> emit8 0xff
x86_fabs    = emit8 0xd9 >> emit8 0xe1
x86_ftst    = emit8 0xd9 >> emit8 0xe4
x86_fxam    = emit8 0xd9 >> emit8 0xe5
x86_fpatan  = emit8 0xd9 >> emit8 0xf3
x86_fprem   = emit8 0xd9 >> emit8 0xf8
x86_fprem1  = emit8 0xd9 >> emit8 0xf5
x86_frndint = emit8 0xd9 >> emit8 0xfc
x86_fsqrt   = emit8 0xd9 >> emit8 0xfa
x86_fptan   = emit8 0xd9 >> emit8 0xf2

-- Fast instruction sequences for 1 to 7-byte noops.

x86_padding :: (Eq t, Num t) => t -> CodeGen e s ()
x86_padding size =
    case size of
      1 -> x86_nop
      2 -> emit8 0x8b >> emit8  0xc0
      3 -> emit8 0x8d >> emit8 0x6d >> emit8 0x00
      4 -> emit8 0x8d >> emit8 0x64 >> emit8 0x24 >> emit8 0x00
      5 -> emit8 0x8d >> emit8 0x64 >> emit8 0x24 >> emit8 0x00 >>
           x86_nop
      6 -> emit8 0x8d >> emit8 0xad >>
           emit8 0x00 >> emit8 0x00 >>
           emit8 0x00 >> emit8 0x00
      7 -> emit8 0x8d >> emit8 0xa4 >>
           emit8 0x24 >> emit8 0x00 >>
           emit8 0x00 >> emit8 0x00 >>
           emit8 0x00
      _ -> failCodeGen (PP.text "invalid padding size")

-- Generate the code for a function prologue.  The frame_size is the
-- number of bytes to be allocated as the frame size, and the reg_mask
-- specifies which registers to save on function entry.

x86_prolog :: Int -> Int -> CodeGen e s ()
x86_prolog frame_size reg_mask =
    do x86_push_reg x86_ebp
       x86_mov_reg_reg x86_ebp x86_esp x86_dword_size
       gen_push 0 1
       if frame_size /= 0
          then x86_alu_reg_imm x86_sub x86_esp frame_size
          else return ()
  where
  gen_push i m =
     if i <= x86_edi
        then do if (reg_mask .&. m) /= 0
                   then x86_push_reg i
                   else return ()
                gen_push (i + 1) (m `shiftL` 1)
        else return ()

-- Opposite to x86_prolog: destroys the stack frame and restores the
-- registers in reg_mask, which should be the same as the register mask
-- used on function entry.

x86_epilog :: Int -> CodeGen e s ()
x86_epilog reg_mask =
    do gen_pop x86_edi (1 `shiftL` (fromIntegral x86_edi))
       x86_mov_reg_reg x86_esp x86_ebp x86_dword_size
       x86_pop_reg x86_ebp
       x86_ret
  where
  gen_pop i m =
    if m /= 0
       then do if (reg_mask .&. m) /= 0
                  then x86_pop_reg i
                  else return ()
               gen_pop (i - 1) (m `shiftR` 1)
       else return ()

-- TODO: Move signatures to definition, delete duplicates.
x86_xchg_reg_reg ::
  (Eq a, Num a) =>
  Word8
  -> Word8
  -> a
  -> CodeGen e s ()
x86_xchg_mem_reg ::
  (Eq a, Num a) =>
  Word32
  -> Word8
  -> a
  -> CodeGen e s ()
x86_xchg_membase_reg ::
  (Eq a, Num a) =>
  Word8
  -> Word32
  -> Word8
  -> a
  -> CodeGen e s ()
x86_xadd_reg_reg ::
  (Eq a, Num a) =>
  Word8
  -> Word8
  -> a
  -> CodeGen e s ()
x86_xadd_mem_reg ::
  (Eq a, Num a) =>
  Word32
  -> Word8
  -> a
  -> CodeGen e s ()
x86_xadd_membase_reg ::
  (Eq a, Num a) =>
  Word8
  -> Word32
  -> Word8
  -> a
  -> CodeGen e s ()
x86_inc_mem ::
  Word32 -> CodeGen e s ()
x86_inc_membase ::
  Word8
  -> Word32
  -> CodeGen e s ()
x86_inc_reg ::
  Word8 -> CodeGen e s ()
x86_dec_mem ::
  Word32 -> CodeGen e s ()
x86_dec_membase ::
  Word8
  -> Word32
  -> CodeGen e s ()
x86_dec_reg ::
  Word8 -> CodeGen e s ()
x86_not_mem ::
  Word32 -> CodeGen e s ()
x86_not_membase ::
  Word8
  -> Word32
  -> CodeGen e s ()
x86_not_reg ::
  Word8 -> CodeGen e s ()
x86_neg_mem ::
  Word32 -> CodeGen e s ()
x86_neg_membase ::
  Word8
  -> Word32
  -> CodeGen e s ()
x86_neg_reg ::
  Word8 -> CodeGen e s ()
x86_alu_mem_imm ::
  Word8
  -> Word32
  -> Word32
  -> CodeGen e s ()
x86_alu_membase_imm ::
  Word8
  -> Word8
  -> Word32
  -> Word32
  -> CodeGen e s ()
x86_alu_membase8_imm ::
  Word8
  -> Word8
  -> Word32
  -> Word8
  -> CodeGen e s ()
x86_alu_mem_reg ::
  Word8
  -> Word32
  -> Word8
  -> CodeGen e s ()
x86_alu_membase_reg ::
  Word8
  -> Word8
  -> Word32
  -> Word8
  -> CodeGen e s ()
x86_alu_reg_reg ::
  Word8
  -> Word8
  -> Word8
  -> CodeGen e s ()
x86_alu_reg8_reg8 ::
  Word8
  -> Word8
  -> Word8
  -> Bool
  -> Bool
  -> CodeGen e s ()
x86_alu_reg_mem ::
  Word8
  -> Word8
  -> Word32
  -> CodeGen e s ()
x86_alu_reg_membase ::
  Word8
  -> Word8
  -> Word8
  -> Word32
  -> CodeGen e s ()
x86_test_reg_imm ::
  Word8
  -> Word32
  -> CodeGen e s ()
x86_test_mem_imm ::
  Word32
  -> Word32
  -> CodeGen e s ()
x86_test_membase_imm ::
  Word8
  -> Word32
  -> Word32
  -> CodeGen e s ()
x86_test_reg_reg ::
  Word8
  -> Word8
  -> CodeGen e s ()
x86_test_mem_reg ::
  Word32
  -> Word8
  -> CodeGen e s ()
x86_test_membase_reg ::
  Word8
  -> Word32
  -> Word8
  -> CodeGen e s ()
x86_shift_reg_imm ::
  Word8
  -> Word8
  -> Word8
  -> CodeGen e s ()
x86_shift_mem_imm ::
  Word8
  -> Word32
  -> Word8
  -> CodeGen e s ()
x86_shift_membase_imm ::
  Word8
  -> Word8
  -> Word32
  -> Word8
  -> CodeGen e s ()
x86_shift_reg ::
  Word8
  -> Word8
  -> CodeGen e s ()
x86_shift_mem ::
  Word8
  -> Word32
  -> CodeGen e s ()
x86_shift_membase ::
  Word8
  -> Word8
  -> Word32
  -> CodeGen e s ()
x86_shrd_reg ::
  Word8
  -> Word8
  -> CodeGen e s ()
x86_shrd_reg_imm ::
  Word8
  -> Word8
  -> Word8
  -> CodeGen e s ()
x86_shld_reg ::
  Word8
  -> Word8
  -> CodeGen e s ()
x86_shld_reg_imm ::
  Word8
  -> Word8
  -> Word8
  -> CodeGen e s ()

-- =============================================================================
-- SSE instructions.
-- =============================================================================

data X86_SSE_PFX = X86_SSE_SD
                 | X86_SSE_SS
                 | X86_SSE_PD
                 | X86_SSE_PS
--newtype X86_SSE_PFX = X86_SSE_PFX (forall e s. CodeGen e s ())

x86_sse_sd, x86_sse_ss, x86_sse_pd, x86_sse_ps :: X86_SSE_PFX
x86_sse_sd = X86_SSE_SD
x86_sse_ss = X86_SSE_SS
x86_sse_pd = X86_SSE_PD
x86_sse_ps = X86_SSE_PS

emit_sse :: X86_SSE_PFX -> CodeGen e s ()
emit_sse X86_SSE_SD = emit8 0xf2
emit_sse X86_SSE_SS = emit8 0xf3
emit_sse X86_SSE_PD = emit8 0x66
emit_sse X86_SSE_PS = return ()

x86_sqrt_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_sqrt_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x51
       x86_reg_emit dreg reg

x86_sqrt_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_sqrt_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x51
       x86_mem_emit dreg mem

x86_sqrt_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_sqrt_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x51
       x86_membase_emit dreg basereg disp

x86_add_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_add_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x58
       x86_reg_emit dreg reg

x86_add_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_add_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x58
       x86_mem_emit dreg mem

x86_add_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_add_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x58
       x86_membase_emit dreg basereg disp

x86_mul_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_mul_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x59
       x86_reg_emit dreg reg

x86_mul_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_mul_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x59
       x86_mem_emit dreg mem

x86_mul_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_mul_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x59
       x86_membase_emit dreg basereg disp

x86_sub_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_sub_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5c
       x86_reg_emit dreg reg

x86_sub_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_sub_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5c
       x86_mem_emit dreg mem

x86_sub_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_sub_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5c
       x86_membase_emit dreg basereg disp

x86_min_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_min_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5d
       x86_reg_emit dreg reg

x86_min_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_min_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5d
       x86_mem_emit dreg mem

x86_min_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_min_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5d
       x86_membase_emit dreg basereg disp

x86_div_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_div_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5e
       x86_reg_emit dreg reg

x86_div_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_div_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5e
       x86_mem_emit dreg mem

x86_div_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_div_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5e
       x86_membase_emit dreg basereg disp

x86_max_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_max_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5f
       x86_reg_emit dreg reg

x86_max_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_max_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5f
       x86_mem_emit dreg mem

x86_max_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_max_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x5f
       x86_membase_emit dreg basereg disp

x86_mov_sse_reg_reg :: X86_SSE_PFX -> Word8 -> Word8 -> CodeGen e s ()
x86_mov_sse_reg_reg pfx dreg reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x10
       x86_reg_emit dreg reg

x86_mov_sse_reg_mem :: X86_SSE_PFX -> Word8 -> Word32 -> CodeGen e s ()
x86_mov_sse_reg_mem pfx dreg mem =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x10
       x86_mem_emit dreg mem

x86_mov_sse_reg_membase :: X86_SSE_PFX -> Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_mov_sse_reg_membase pfx dreg basereg disp =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x10
       x86_membase_emit dreg basereg disp

x86_mov_sse_mem_reg :: X86_SSE_PFX -> Word32 -> Word8 -> CodeGen e s ()
x86_mov_sse_mem_reg pfx mem reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x11
       x86_mem_emit reg mem

x86_mov_sse_membase_reg :: X86_SSE_PFX -> Word8 -> Word32 -> Word8 -> CodeGen e s ()
x86_mov_sse_membase_reg pfx basereg disp reg =
    do emit_sse pfx
       emit8 0x0f
       emit8 0x11
       x86_membase_emit reg basereg disp

x86_ucomisd_reg_reg :: Word8 -> Word8 -> CodeGen e s ()
x86_ucomisd_reg_reg dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x2e
       x86_reg_emit dreg reg

x86_ucomisd_reg_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_ucomisd_reg_mem dreg mem =
    do emit8 0x66
       emit8 0x0f
       emit8 0x2e
       x86_mem_emit dreg mem

x86_ucomisd_reg_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_ucomisd_reg_membase dreg basereg disp =
    do emit8 0x66
       emit8 0x0f
       emit8 0x2e
       x86_membase_emit dreg basereg disp

x86_ucomiss_reg_reg :: Word8 -> Word8 -> CodeGen e s ()
x86_ucomiss_reg_reg dreg reg =
    do emit8 0x0f
       emit8 0x2e
       x86_reg_emit dreg reg

x86_ucomiss_reg_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_ucomiss_reg_mem dreg mem =
    do emit8 0x0f
       emit8 0x2e
       x86_mem_emit dreg mem

x86_ucomiss_reg_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_ucomiss_reg_membase dreg basereg disp =
    do emit8 0x0f
       emit8 0x2e
       x86_membase_emit dreg basereg disp

x86_comisd_reg_reg :: Word8 -> Word8 -> CodeGen e s ()
x86_comisd_reg_reg dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x2f
       x86_reg_emit dreg reg

x86_comisd_reg_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_comisd_reg_mem dreg mem =
    do emit8 0x66
       emit8 0x0f
       emit8 0x2f
       x86_mem_emit dreg mem

x86_comisd_reg_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_comisd_reg_membase dreg basereg disp =
    do emit8 0x66
       emit8 0x0f
       emit8 0x2e
       x86_membase_emit dreg basereg disp

x86_comiss_reg_reg :: Word8 -> Word8 -> CodeGen e s ()
x86_comiss_reg_reg dreg reg =
    do emit8 0x0f
       emit8 0x2f
       x86_reg_emit dreg reg

x86_comiss_reg_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_comiss_reg_mem dreg mem =
    do emit8 0x0f
       emit8 0x2f
       x86_mem_emit dreg mem

x86_comiss_reg_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_comiss_reg_membase dreg basereg disp =
    do emit8 0x0f
       emit8 0x2e
       x86_membase_emit dreg basereg disp


newtype XMMReg = XMMReg Word8
    deriving (Eq, Ord)

newtype Mem = Mem Word32

data MemBase = MemBase Word8 Word32


class XMMLocation xmm where
   xmm_location_emit :: Word8 -> xmm -> CodeGen e s ()

instance XMMLocation XMMReg where
   xmm_location_emit dreg (XMMReg reg) =
      x86_reg_emit dreg reg

instance XMMLocation Mem where
   xmm_location_emit dreg (Mem mem) =
      x86_mem_emit dreg mem

instance XMMLocation MemBase where
   xmm_location_emit dreg (MemBase basereg disp) =
      x86_membase_emit dreg basereg disp


x86_movss_to_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movss_to_reg dreg reg =
    do emit8 0xf3
       emit8 0x0f
       emit8 0x10
       xmm_location_emit dreg reg

x86_movss_from_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movss_from_reg dreg reg =
    do emit8 0xf3
       emit8 0x0f
       emit8 0x11
       xmm_location_emit dreg reg

x86_movsd_to_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movsd_to_reg dreg reg =
    do emit8 0xf2
       emit8 0x0f
       emit8 0x10
       xmm_location_emit dreg reg

x86_movsd_from_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movsd_from_reg dreg reg =
    do emit8 0xf2
       emit8 0x0f
       emit8 0x11
       xmm_location_emit dreg reg


-- | xmm must not be a register
x86_movlps_to_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movlps_to_reg dreg reg =
    do emit8 0x0f
       emit8 0x12
       xmm_location_emit dreg reg

-- | xmm must not be a register
x86_movlps_from_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movlps_from_reg dreg reg =
    do emit8 0x0f
       emit8 0x13
       xmm_location_emit dreg reg

-- | xmm must not be a register
x86_movlpd_to_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movlpd_to_reg dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x12
       xmm_location_emit dreg reg

-- | xmm must not be a register
x86_movlpd_from_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movlpd_from_reg dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x13
       xmm_location_emit dreg reg


x86_movups_to_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movups_to_reg dreg reg =
    do emit8 0x0f
       emit8 0x10
       xmm_location_emit dreg reg

x86_movups_from_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movups_from_reg dreg reg =
    do emit8 0x0f
       emit8 0x11
       xmm_location_emit dreg reg

x86_movupd_to_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movupd_to_reg dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x10
       xmm_location_emit dreg reg

x86_movupd_from_reg :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_movupd_from_reg dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x11
       xmm_location_emit dreg reg


x86_haddps :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_haddps dreg reg =
    do emit8 0xf2
       emit8 0x0f
       emit8 0x7c
       xmm_location_emit dreg reg

x86_haddpd :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_haddpd dreg reg =
    do emit8 0x66
       emit8 0x0f
       emit8 0x7c
       xmm_location_emit dreg reg


x86_shufps :: XMMLocation xmm => Word8 -> xmm -> Word8 -> CodeGen e s ()
x86_shufps dreg reg src =
    do emit8 0x0f
       emit8 0xc6
       xmm_location_emit dreg reg
       emit8 src

x86_shufpd :: XMMLocation xmm => Word8 -> xmm -> Word8 -> CodeGen e s ()
x86_shufpd dreg reg src =
    do emit8 0x66
       emit8 0x0f
       emit8 0xc6
       xmm_location_emit dreg reg
       emit8 src


x86_cvtdq2ps :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_cvtdq2ps dreg reg =
    do emit8 0x0f
       emit8 0x5b
       xmm_location_emit dreg reg

x86_cvttps2dq :: XMMLocation xmm => Word8 -> xmm -> CodeGen e s ()
x86_cvttps2dq dreg reg =
    do emit8 0xf3
       emit8 0x0f
       emit8 0x5b
       xmm_location_emit dreg reg



-- =============================================================================
-- Prefetching instructions.
-- =============================================================================

x86_prefetch0_mem :: Word32 -> CodeGen e s ()
x86_prefetch0_mem m = x86_prefetch_mem 1 m

x86_prefetch1_mem :: Word32 -> CodeGen e s ()
x86_prefetch1_mem m = x86_prefetch_mem 2 m

x86_prefetch2_mem :: Word32 -> CodeGen e s ()
x86_prefetch2_mem m = x86_prefetch_mem 3 m

x86_prefetchnta_mem :: Word32 -> CodeGen e s ()
x86_prefetchnta_mem m = x86_prefetch_mem 0 m

x86_prefetch_mem :: Word8 -> Word32 -> CodeGen e s ()
x86_prefetch_mem hint disp =
    do emit8 0x0f
       emit8 0x18
       x86_address_byte 0 hint 0
       x86_imm_emit32 disp

x86_prefetch0_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_prefetch0_membase r m = x86_prefetch_membase 1 r m

x86_prefetch1_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_prefetch1_membase r m = x86_prefetch_membase 2 r m

x86_prefetch2_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_prefetch2_membase r m = x86_prefetch_membase 3 r m

x86_prefetchnta_membase :: Word8 -> Word32 -> CodeGen e s ()
x86_prefetchnta_membase r m = x86_prefetch_membase 0 r m

x86_prefetch_membase :: Word8 -> Word8 -> Word32 -> CodeGen e s ()
x86_prefetch_membase hint reg disp =
    do emit8 0x0f
       emit8 0x18
       x86_membase_emit hint reg disp

x86_prefetch0_regp :: Word8 -> CodeGen e s ()
x86_prefetch0_regp r = x86_prefetch_regp 1 r

x86_prefetch1_regp :: Word8 -> CodeGen e s ()
x86_prefetch1_regp r = x86_prefetch_regp 2 r

x86_prefetch2_regp :: Word8 -> CodeGen e s ()
x86_prefetch2_regp r = x86_prefetch_regp 3 r

x86_prefetchnta_regp :: Word8 -> CodeGen e s ()
x86_prefetchnta_regp r = x86_prefetch_regp 0 r

x86_prefetch_regp :: Word8 -> Word8 -> CodeGen e s ()
x86_prefetch_regp hint reg =
    do emit8 0x0f
       emit8 0x18
       x86_regp_emit hint reg

