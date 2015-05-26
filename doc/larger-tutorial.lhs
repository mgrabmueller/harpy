\documentclass[a4paper,11pt]{article}

\usepackage[margin=2.5cm]{geometry}
\usepackage{hyperref}

%include polycode.fmt
%format alpha = "\alpha"


\title{\textbf{Slightly Larger Harpy Tutorial}}
\author{Martin Grabmüller \and Dirk Kleeblatt}

\begin{document}
\maketitle
\begin{abstract}\noindent
This tutorial is an introduction to some of the more advanced features
of Harpy.  In the course of the tutorial, we will build a small
run-time compiler for a call-by-value lambda-calculus.
This document is written as a literate Haskell program, so you can
compile and run it without any modifications.  
\end{abstract}

\section{Introduction}

In this tutorial, we develop a small run-time compiler for a simple
call-by-value lambda calculus extended with natural number constants.

Before reading, you should have read the Harpy tutorial, which
introduces the most basic concepts.  We also recommend to have a
running \verb|ghci| session running for loading the literate Haskell
script of this tutorial and to try it out.  Maybe you want to have the
Harpy API documentation available, too.

\section{The Code}

To make use of Harpy and (a subset of) the x86 assembler instructions, we need
to import some modules.

\begin{code}
import Harpy.CodeGenMonad
import Harpy.X86Assembler
import Harpy.X86Disassembler
import Foreign
import Control.Monad
import Control.Monad.Trans
import Data.List 
\end{code}

The following module system hackery is necessary, because the
|Harpy.X86Assembler| module exports a method called
|div|, which clashes with the |div| function
exported by the Haskell Prelude.  The prelude |div| will be
hidden, but is still available as a qualified name.

\begin{code}
import Prelude hiding (div)
import qualified Prelude
\end{code}

\subsection{Data Types}

In the following, we define several data types for representing lambda
calculus terms

A |Name| represents a variable name.  In a real
implementation, it would probably be more complicated data type for
handling qualified or compiler-generated names.  For now, a string
will suffice.

\begin{code}
type Name = String
\end{code}

In our implementation, a variable may refer to (a) a global variable,
which is represented by its address, (b) a parameter, which is
represented by the number of enclosing lambda expression, or (c) a
free variable, which is represented by its index into the current code
closure.

\begin{code}
data VarLoc  =  Global Word32
	     |  Param Int
             |  Free Int
             deriving (Show)
\end{code}

A compile-time environment represents a mapping from variable names to
variable locations.

\begin{code}
data Env = Env [(Name, VarLoc)]
\end{code}

An expression is represented by the following data type and may be a
variable, an immediate value (integer constant), a lambda abstraction
or an application expression.

\begin{code}
data Exp  =  Var Name
	  |  Imm Int
          |  Abs Name Exp
          |  App Exp Exp
\end{code}

The |State| type represents the state of a running program.
It consists of a pointer to the next available word of heap memory,
and a pointer to the end of the heap.  These two values are necessary
for dynamically allocating memory and for performing heap-overflow
checks.

\begin{code}
data State = State {  heap     :: Ptr Word32,
		      heapEnd  :: Ptr Word32}
\end{code}

The following test expressions are constructed using the constructors of
type |Exp| and are to be evaluated by the main program.

\begin{code}
testExp0 = App (Abs "x" (Var "x")) (Imm 12)
testExp1 = App (App (Abs "x" (Abs "y" (Var "x"))) (Imm 12)) (Imm 23)
\end{code}

\subsection{Main Program}

Before presenting the compiler, we will have a look at how it is used.
The entry point to the dynamic compiler is the function
|interpret|, which takes an expression and returns the value
calculated by the expression.  The main program simply prints the
result.

\begin{code}
$(callDecl "callFunc" [t|Word32|])
\end{code}

\begin{code}
main :: IO ()
main = do  res <- interpret testExp1
           putStrLn $ "Result: " ++ show res
\end{code}

The |interpret| function allocates 16 kbytes of heap and
compiles the expression.  The |State| of the compiler is
passed to the |runCodeGen| function as a user state.

\begin{code}
interpret :: Exp -> IO Word32
interpret exp =
    do  let heapSize = 1024 * 16
        heap <- mallocBytes heapSize
        (_, Right res) <- runCodeGen (compileTop exp) (Env []) 
	 		  (State heap (heap `plusPtr` 
				       (heapSize `Prelude.div` 4)))
        return res
\end{code}

\subsection{Compilation}

The compilation starts in the function |compileTop|, which emits code
for creating a stack frame, saving all general-purpose registers
(except \verb|eax|, which will contain the result of the execution),
and finally compiles the expression.  Afterwards, all registers are
restored and the function is called.  In addition we call the
disassembler on the generated code and print it out.  This is a useful
debuggin aid when generating assembler code.

\begin{code}
compileTop :: Exp -> CodeGen Env State Word32
compileTop exp =
    do  st <- getState
        push ebp
        mov ebp esp
        push ebx
        push ecx
        push edi
        push esi
        mov ecx ((fromIntegral (ptrToWordPtr (heap st))) :: Word32)
        compile exp
        pop esi
        pop edi
        pop ecx
        pop ebx
        mov esp ebp
        pop ebp
        ret
        y <- callFunc
        dis <- disassemble
        liftIO $ mapM_ (putStrLn . showIntel) dis
        return y

\end{code}

\subsubsection{Handling Variables}

The |findVar| function looks up a variable in the
environment and returns its binding.

\begin{code}
findVar s = 
    do  Env e <- getEnv
        case lookup s e of
          Just vl -> return vl
	  Nothing -> error ("unbound variable: " ++ s)
\end{code}

For the implementation of closures, we need to calculate free
variables of expressions, so that we know what to store into closures
and how to access free variables from the body of a lambda expression.

\begin{code}
freeVariables (Var s) = return [s]
freeVariables (Imm _) = return []
freeVariables (Abs x b) = liftM (delete x) (freeVariables b)
freeVariables (App e1 e2) = liftM2 union (freeVariables e1) (freeVariables e2)
\end{code}

\subsubsection{Heap Allocation}

Closures are the only dynamically allocated structures in our little
compiler.  They are allocated from a contiguous block of malloc'ed
memory, which is passed in the code generator monad's state.  The
following function generates code for allocating $w$ words of memory
in the heap, and returning a pointer to the memory in register
\verb|eax|.  On heap overflow, a breakpoint instruction is executed.

\begin{code}
emitAlloc w =
    do  mov eax ecx
        st <- getState
        cmp ecx (fromIntegral (ptrToWordPtr (heapEnd st)) :: Word32)
        next <- newLabel
        jl next
        breakpoint
        defineLabel next
        add ecx ((4 * w) :: Word32)
\end{code}

\subsubsection{The Compilation Proper}

Finally, the |compile| function compiles an expression.
Constants are simply loaded into the result register, values of
variables are retrieved from their respective addresses (global
variables from absolute addresses, parameters relative to the stack
base pointer, and free variables relative to the closure pointer.

\begin{code}
compile :: Exp -> CodeGen Env State ()
compile (Imm i) = 
    do  mov eax (fromIntegral i :: Word32)
compile (Var s) =
    do  c <- findVar s
        case c of
          Global addr -> mov eax (Addr addr)
	  Param ofs -> mov eax (Disp (fromIntegral ofs), ebp)
	  Free ofs -> mov eax (Disp (fromIntegral ofs), esi)
\end{code}

Lambda expressions are more complicated.  We first need to calculate
the set of free variables of the body expression.  Then the body of
the expression is compiled (with a jump around its code, so that it is
not actually executed at this point).  Then a closure record is
allocated on the heap, a code pointer to the body is stored in the
first word, and the values of all free variables of the body are
calculated and moved into the remaining fields of the closure.

The result is a pointer to the new closure, returned in \verb|eax|, as
usual.

\begin{code}
compile exp@(Abs x body) =
    do  fv <- freeVariables exp
        let frees = zip fv (map Free [4,8..])
        next <- newLabel
        jmp next
        f <- setLabel
        push ebp
        mov ebp esp
        mov esi (Disp 8, ebp)
        withEnv (Env ((x, Param 12) : frees)) (compile body)
        mov esp ebp
        pop ebp
        ret
        defineLabel next
        emitAlloc (fromIntegral (length frees + 1))
        mov edi eax
        mov (Disp 0, edi) f
        if length frees > 0
           then do  push eax
                    mapM_ (\ (x, Free ofs) -> 
                           do  compile (Var x)
                               mov (Disp (fromIntegral ofs), edi) eax)
                      frees
                    pop eax
           else return ()
compile (App e1 e2) = 
    do  compile e2
        push eax
        compile e1
        push eax
        call (Disp 0, eax)
        add esp (8 :: Word32)
\end{code}

\section{Conclusion}

This completes our little just-in-time compiled lambda calculus
evaluator.  We hope that this tutorial makes clear many of the fine
points in using the library.

Happy Harpy Hacking in Haskell!

\end{document}
