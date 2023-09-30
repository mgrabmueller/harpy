--------------------------------------------------------------------------
-- |
-- Module      :  X86CodeGen
-- Copyright   :  (c) 2006-2015 Martin Grabmueller and Dirk Kleeblatt
-- License     :  BSD3
--
-- Maintainer  :  martin@grabmueller.de
-- Stability   :  quite experimental
-- Portability :  portable (but generated code non-portable)
--
-- This module exports several combinators for writing loops,
-- conditionals and function prolog\/epilog code.
--
-- Note: this module is under heavy development and the exported API
-- is definitely not yet stable.
--------------------------------------------------------------------------

module Harpy.X86CGCombinators(
  -- * Types
  UserState(..),
  UserEnv(..),
  emptyUserEnv,
  emptyUserState,
  CtrlDest(..),
  DataDest(..),
  -- * Combinators
  ifThenElse,
  doWhile,
  continue,
  continueBranch,
  saveRegs,
  function,
  withDataDest,
  withCtrlDest,
  withDest,
   ) where

import Text.PrettyPrint.HughesPJ

import Foreign
import Data.Word

import Harpy.CodeGenMonad
import Harpy.X86CodeGen
import Harpy.X86Assembler

-- | Destination for a calculated value.
data DataDest = RegDest Reg32            -- ^ Store into specific register
              | StackDest                -- ^ Push onto stack
              | MemBaseDest Reg32 Word32 -- ^ Store at memory address
              | Ignore                   -- ^ Throw result away.

-- | Destination for control transfers
data CtrlDest = FallThrough              -- ^ Go to next instruction
              | Return                   -- ^ Return from current functio
              | Goto Label               -- ^ Go to specific label
              | Branch CtrlDest CtrlDest -- ^ Go to one of the given labels
                                         -- depending on outcome of test

-- | User state is used to maintain bitmask of registers currently in use.
data UserState = UserState {}


-- | User environment stores code generators for accessing specific
-- variables as well as the current data and control destinations
data UserEnv = UserEnv { bindings :: [(String, CodeGen UserEnv UserState ())],
                         dataDest :: DataDest,
                         ctrlDest :: CtrlDest }

emptyUserState :: UserState
emptyUserState = UserState{}

emptyUserEnv :: UserEnv
emptyUserEnv = UserEnv{bindings = [], dataDest = Ignore,
                       ctrlDest = Return}

ifThenElse :: CodeGen UserEnv s r
	      -> CodeGen UserEnv s a
	      -> CodeGen UserEnv s a1
	      -> CodeGen UserEnv s ()
ifThenElse condCg thenCg elseCg =
    do env <- getEnv
       elseLabel <- newLabel
       endLabel <- newLabel
       withDest Ignore (Branch FallThrough (Goto elseLabel)) condCg
       withCtrlDest (case ctrlDest env of
                       FallThrough -> Goto endLabel
                       _ -> ctrlDest env)
                    (thenCg >> continue)
       elseLabel @@ (elseCg >> continue)
       endLabel @@ return ()

doWhile :: CodeGen UserEnv s r -> CodeGen UserEnv s a -> CodeGen UserEnv s ()
doWhile condCg bodyCg =
    do topLabel <- newLabel
       testLabel <- newLabel
       jmp testLabel
       topLabel @@ withCtrlDest FallThrough (bodyCg >> continue)
       testLabel @@ withDest Ignore (Branch (Goto topLabel) FallThrough)
                        condCg
       continue

doFor :: (Mov a Word32, Add a Word32, Cmp a Word32) => a -> Word32 -> Word32 -> Int32 ->
         CodeGen UserEnv s r ->
         CodeGen UserEnv s ()
doFor loc from to step body =
    do topLabel <- newLabel
       testLabel <- newLabel
       mov loc from
       jmp testLabel
       topLabel @@ withCtrlDest FallThrough  (body >> continue)
       testLabel @@ cmp loc to
       add loc (fromIntegral step :: Word32)
       if step < 0
          then jge topLabel
          else jle topLabel
       continue


continue :: CodeGen UserEnv s ()
continue =
    do env <- getEnv
       cont (ctrlDest env)
  where
  cont FallThrough = return ()
  cont (Goto l) = jmp l
  cont (Branch _ _) = error "Branch in continue"
  cont Return = x86_epilog 0


continueBranch :: Int -> Bool -> CodeGen UserEnv s ()
continueBranch cc isSigned =
    do env <- getEnv
       let Branch c1 c2 = ctrlDest env
       cont cc isSigned c1 c2
  where
  cont cc isSigned (Goto l1) (Goto l2) =
    do x86_branch32 cc 0 isSigned
       emitFixup l1 (-4) Fixup32
       x86_branch32 (negateCC cc) 0 isSigned
       emitFixup l2 (-4) Fixup32
  cont cc isSigned (Goto l1) FallThrough =
    do x86_branch32 cc 0 isSigned
       emitFixup l1 (-4) Fixup32
  cont cc isSigned FallThrough (Goto l2) =
    do x86_branch32 (negateCC cc) 0 isSigned
       emitFixup l2 (-4) Fixup32
  cont cc isSigned (Goto l1) Return =
    do x86_branch32 cc 0 isSigned
       emitFixup l1 (-4) Fixup32
       withCtrlDest Return continue
  cont cc isSigned Return (Goto l2) =
    do x86_branch32 (negateCC cc) 0 isSigned
       emitFixup l2 (-4) Fixup32
       withCtrlDest Return continue
  cont _ _ _ _ = error "unhandled case in continueBranch"

reg sreg =
    do env <- getEnv
       reg' sreg (dataDest env)
  where
  reg' sreg (RegDest r) =
    do if sreg /= r
          then mov r sreg
          else return ()
  reg' sreg (StackDest) =
    do push sreg
  reg' sreg (MemBaseDest r offset) =
    do mov (Disp offset, r) sreg
  reg' sreg Ignore = return ()

membase reg ofs =
    do env <- getEnv
       membase' reg ofs (dataDest env)
  where
  membase' reg ofs (RegDest r) =
    do mov r (Disp ofs, reg)
  membase' reg ofs (StackDest) =
    do push (Disp ofs, reg)
  membase' reg ofs (MemBaseDest r offset) =
    do push edi
       mov edi (Disp ofs, reg)
       mov (Disp offset, r) edi
       pop edi
  membase' reg ofs Ignore = return ()

global ofs =
    do env <- getEnv
       global' ofs (dataDest env)
  where
  global' ofs (RegDest r) =
    do mov r (Addr ofs)
  global' ofs (StackDest) =
    do push (Addr ofs)
  global' ofs (MemBaseDest r offset) =
    do push edi
       mov edi (Addr ofs)
       mov (Disp offset, r) edi
       pop edi
  global' ofs Ignore = return ()

immediate value =
    do env <- getEnv
       immediate' value (dataDest env)
  where
  immediate' value (RegDest r) =
    do mov r value
  immediate' value (StackDest) =
    do x86_push_imm value
  immediate' value (MemBaseDest r offset) =
    do push edi
       mov edi value
       mov (Disp offset, r) edi
       pop edi
  immediate' ofs Ignore = return ()

-- | Save a number of registers on the stack, perform the given code
-- generation, and restore the registers.
saveRegs :: [Reg32] -> CodeGen UserEnv s r -> CodeGen UserEnv s ()
saveRegs [] cg = cg >> return ()
saveRegs regs cg =
    do gen_push regs
       withCtrlDest FallThrough cg
       gen_pop regs
       continue
  where
  gen_push [] = return ()
  gen_push (r:regs) = push r >> gen_push regs
  gen_pop [] = return ()
  gen_pop (r:regs) = gen_pop regs >> pop r

-- | Perform the code generation associated with the variable given.
loadVar :: String -> CodeGen UserEnv UserState ()
loadVar name =
    do UserEnv{bindings = assoc} <- getEnv
       case lookup name assoc of
         Just cg -> cg
         Nothing -> failCodeGen (text ("undefined variable: " ++ name))

-- | Set the data destinations to the given values while
-- running the code generator.
withDataDest :: DataDest -> CodeGen UserEnv s r -> CodeGen UserEnv s r
withDataDest ddest cg =
    do env <- getEnv
       withEnv (env{dataDest = ddest}) cg

-- | Set the control destinations to the given values while
-- running the code generator.
withCtrlDest :: CtrlDest -> CodeGen UserEnv s r -> CodeGen UserEnv s r
withCtrlDest cdest cg =
    do env <- getEnv
       withEnv (env{ctrlDest = cdest}) cg

-- | Set the data and control destinations to the given values while
-- running the code generator.
withDest :: DataDest -> CtrlDest -> CodeGen UserEnv s r -> CodeGen UserEnv s r
withDest ddest cdest cg =
    do env <- getEnv
       withEnv (env{dataDest = ddest, ctrlDest = cdest}) cg

-- | Emit the necessary function prolog and epilog code and invoke the
-- given code generator for the code inbetween.
function :: CodeGen UserEnv s r -> CodeGen UserEnv s r
function cg =
    do x86_prolog 0 0
       withDest (RegDest eax) Return $ cg
