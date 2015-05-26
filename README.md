# harpy - Runtime code generation for x86 machine code

    Codename: Harpy - Haskell Assembler at Run-time produces Y...
      Harpy [myth.]	f: die Harpyie
      http://en.wikipedia.org/wiki/Harpy

## Introduction

Harpy is a library for run-time code generation in Haskell programs.

Harpy requires several Haskell extensions and GHC-specific features
(the Haskell FFI, Template Haskell, multi-parameter type classes and
monad transformers).

## Features

The following modules are included in this package:

Harpy.CodeGenMonad: This module defines the code generator monad,
  which is a combined state/reader/exception monad.  It contains
  all the necessary details for allocating and managing code buffers.

Harpy.X86CodeGen: This module contains all the functions for generating
  native x86 machine code.  The functions are very simple, and it is
  necessary to specify all addressing modes etc. when emitting an
  instruction.

Harpy.X86Assembler: A type class based layer on top of X86CodeGen
  which determines the addressing modes from the types of the
  operands.

Harpy.X86CGCombinators: Code generation combinators for conditionals,
  loops, function entry/exit code etc.

Harpy.X86Disassembler: A disassembler for x86 machine code.

Harpy.Call: Exports functions for invoking the generated code.

## Notes about the implementation

### X86CodeGen.lhs

The file X86CodeGen.lhs is based on a header file called x86-codegen.h
from the Mono distribution, which defines macros for emitting x86
machine code directly into a memory buffer.  The Haskell module is a
nearly one-to-one mapping from the original macros to Haskell
functions.  The main differences are:

- Instead of emitting the data directly into a buffer, it uses the
  CodeGen monad from file CodeGenMonad.lhs.

- The functions are strongly typed.

Several things should be kept in mind when using this file:

- Buffer overflow checks have to be done manually with checkBufferSize or
  ensureBufferSize

- MMX, SSE, SSE2 and SSE3 instructions and registers are not supported.

- 64-bit mode is not supported.

- The disassembler supports (in principle) 64-bit mode and SSE
  instructions, but this has not been tested.
