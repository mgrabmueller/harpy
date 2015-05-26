\documentclass[a4paper]{article}

\usepackage[latin1]{inputenc}

\usepackage{listings}

\lstnewenvironment{code}{}{}
\lstset{
  basicstyle=\ttfamily,
  keywordstyle=\normalfont\bfseries,
  identifierstyle=\bfseries,
  commentstyle=\rmfamily,
  texcl=true, 
  language=Haskell,
  flexiblecolumns=false,
  basewidth={0.5em,0.45em},
  extendedchars=true, 
  frame=leftline,
  numbers=left,
  firstnumber=last,
  literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
           {>}{{$>$}}1 {<}{{$<$}}1 
           {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
           {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 {\ .}{{ $\circ$}}2,
  numberstyle=\tiny
} 

\title{Harpy Tutorial}
\author{Martin Grabmüller \and Dirk Kleeblatt}

\begin{document}
\maketitle
\abstract{
We present some of the features of
Harpy, an in-line assembler for Haskell.  This little tutorial shows how to
write assembler programs without making one's hands dirty, staying in the
beautiful pure functional world of Haskell.  During this tutorial, we develop
step by step an assembler implementation of the factorial function, and show
how assembler code can be called from ordinary Haskell code.

This document is written as a literate Haskell program, so you can compile and run it
without any modifications.
}

\section{Introduction}

To make use of Harpy and (a subset of) the x86 assembler instructions, we need
to import some modules.

\begin{code}
import Harpy.CodeGenMonad
import Harpy.X86Assembler
import Foreign
import Control.Monad.Trans
\end{code}

The module \lstinline-Harpy.CodeGenMonad- defines the polymorphic type
\lstinline-CodeGen e s-, which is an instance of the \lstinline-Monad-
class. The type parameters \lstinline-e- and \lstinline-s- can be instantiated
to the type of an user's environment and state, respectively. These behave
like the environments and states known from the \lstinline-Reader- and
\lstinline-State- monads. Besides this monadic type, this module defines some
functions to make use of code labels, and provides an interface to a
disassembler.

The module \lstinline-Harpy.X86Assembler- provides a subset of the x86
assembler instructions, e.~g. \verb-mov- for moving memory words around. These
instructions are implemented as class methods, to allow different addressing
modes without hassle.

We additionally import the module \lstinline-Foreign-, since we need some low
level types to exchange parameters and results with our assembler code, and
\lstinline-Control.Monad.MonadTrans- to have some instances available.

\section{A fast factorial function}

Now we are ready to define the factorial function in assembler code.

\lstset{firstnumber=4}
\begin{code}
fac :: CodeGen e s ()
fac = do loopTest  <- newLabel
         loopStart <- newLabel
         ensureBufferSize 160
         push ecx
         mov  ecx (Disp 8, esp)
         mov  eax (1 :: Word32)
         jmp  loopTest
         loopStart @@ mul ecx
         sub  ecx (1 :: Word32)
         loopTest @@ cmp ecx (0 :: Word32)
         jne  loopStart
         pop  ecx
         ret
\end{code}

We first create two labels, \lstinline-loopTest- and \lstinline-loopStart-, to
mark the test and the beginning of a loop. In lines 5 and 6, these labels are
merely announced to Harpy, they are not (yet) defined to sit at a specific
code position.

Line 8 saves the \lstinline-ecx- rigister on the system stack, because we will
use it as a loop counter, and want to restore it before returning to Haskell
functions.

Line 9 shows an indirect adressing with displacement. Note, that all Harpy
functions use Intel assembler style, i.~e. the first operand is the
destination and the second one the source of each instruction. So this line
moves the memory contents at address \lstinline-esp+8- into
\lstinline-ecx-. Since we will make a C call into our assembler code, this
accesses the first parameter on the stack. When returning to the Haskell world
via \lstinline-ret-, we leave our result in \lstinline-eax-, again adhering to
the C calling convention.

The rest of \lstinline-fac- just accumulates the factorial in \lstinline-eax-
while counting down \lstinline-ecx-. Lines 12 and 14 show how our labels
\lstinline-loopStart- and \lstinline-loopTest- are placed at specific code
positions.

The function \lstinline-fac- is not really our wanted factorial
function. Instead it is a monadic command that, when executed, writes
assembler code into a buffer. To ensure, that this buffer is always large
enough to hold the generated instruction, you have to sprinkle your code with
calls to \lstinline-ensureBufferSize-. In line 7 we make sure that 160 bytes
are available, which is enough for our 10 instructions. As a rule of thumb, no
instruction can be larger than 16 bytes, so the number of assembler
instructions times 16 is a safe upper bound.

The next section shows how to prepare a call into such a buffer.

\section{Preparing a call}

The module \lstinline-Harpy.Call- defines some functions to call functions
written in assembler with various argument and result types. But since it is
possible to use all types suitable for FFI calls as argument or result, sooner
or later you will need some combination not yet implemented. So here we show
how to define your own calling stub.

\lstset{firstnumber=18}
\begin{code}
$(callDecl "callFac" [t|Word32 -> Word32|])
\end{code}

The Template Haskell function \lstinline-callDecl- is used to declare a
function \lstinline-callFac- which will call our assembler fragment. We want
to pass a parameter of type \lstinline-Word32-, and expect a result of the
same type, that's why we give \lstinline+Word32 -> Word32+ as argument to
\lstinline-callDecl-. If you wonder about the fancy \lstinline-$- and
\lstinline-[t| |]-, either look them up in the Template Haskell
documentation, or just ignore them. However, to make this line compile, you
have to switch on Template Haskell, which is done for the Glasgow Haskell
compiler by the command line flag \verb+-fth+.

\section{Calling \lstinline-fac-}

Now we have all we need to call into our factorial function.

\lstset{firstnumber=19}
\begin{code}
runFac :: CodeGen e s ()
runFac = do fac
            x <- liftIO readLn
            y <- callFac x
            liftIO (putStrLn (show y))
\end{code}

We first call \lstinline-fac- to write our assembler code into the internal
buffer. Since the \lstinline-CodeGen- monad ist an instance of
\lstinline-MonadIO-, we can use \lstinline-liftIO- to use all commands we wish
from the \lstinline-IO- monad. Here, we use this to read the argument to the
factorial function from the keyboard, and to write the result back to the
screen. Line 21 calls into the internal code buffer with our assembler
instructions using the stub declared in the last section.

\section{How to use it}

Up to now, all our functions live in the \lstinline-CodeGen- monad. To make
use of them, we have to \emph{unlift} them into the \lstinline-IO- monad. This
is done by \lstinline-runCodeGen-.

\lstset{firstnumber=24}
\begin{code}
main :: IO ()
main = do
   (finalState, result) <- runCodeGen runFac () ()
   case result of
     Right () -> return ()
     Left err -> putStrLn (show err)
\end{code}

The second and third arguments to \lstinline-runCodeGen- are the initial
environment and state. Since we did not use them, their type is polymorphic
and we can use \lstinline-()- as initial values. The result is a pair
consisting of the final state, and a result value. A value constructed with
the constructor \lstinline-Right- indicates a successful run, while
\lstinline-Left- values indicate runtime errors. These might occur for
instance because of infeasible addressing modes.

 \end{document}
