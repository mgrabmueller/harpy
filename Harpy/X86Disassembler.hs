--------------------------------------------------------------------------
-- |
-- Module      :  Harpy.X86Disassembler
-- Copyright   :  (c) Martin Grabmueller and Dirk Kleeblatt
-- License     :  BSD3
--
-- Maintainer  :  martin@grabmueller.de
-- Stability   :  provisional
-- Portability :  portable
--
-- Disassembler for x86 machine code.
--
-- This is a module for compatibility with earlier Harpy releases.  It
-- re-exports the disassembler from the disassembler package.
--------------------------------------------------------------------------

module Harpy.X86Disassembler(
  -- * Types
  Opcode,
  Operand(..),
  InstrOperandSize(..),
  Instruction(..),
  ShowStyle(..),
  -- * Functions
  disassembleBlock,
  disassembleList,
  disassembleArray,
  showIntel,
  showAtt
  ) where

import Text.Disassembler.X86Disassembler
