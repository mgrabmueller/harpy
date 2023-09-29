module Main(main) where

import ArithTypes
import ArithParser

import Harpy
import Harpy.X86Disassembler

import Foreign

import Control.Monad

import System.Console.Readline

import Text.ParserCombinators.Parsec

$(callDecl "callAsWord32" [t|Word32 -> IO Word32|])

main :: IO ()
main = do putStrLn "\n\n\n\nHarpy Interpreter"
          putStrLn "(type :help to see a help message)"
          allocaArray 26 (\ p -> mapM_ (\ i -> poke (advancePtr p i) 0) [0..25] >> repl p False)

repl :: Ptr Int32 -> Bool -> IO ()
repl env verbose =
    do s <- readline "@ "
       case s of
         Nothing -> return ()
         Just s' -> do addHistory s'
                       interpret env verbose s'

interpret :: Ptr Int32 -> Bool -> String -> IO ()
interpret env verbose s =
    do let e = parse statement "<standard input>" s
       case e of
         Left err -> do putStrLn (show err)
                        repl env verbose
         Right stmt -> run env verbose stmt

run :: Ptr Int32 -> Bool -> Stmt -> IO ()
run env verbose (Cmd Help) =
    do putStrLn "Enter an arithmetic expression to evaluate it"
       putStrLn "  e.g. 5 / 2"
       putStrLn "Enter an assignment to set a variable"
       putStrLn "  e.g. a := 4 * 2 - (6 + 1)"
       putStrLn "Enter :help to see this message again"
       putStrLn "Enter :quit to exit"
       putStrLn "Enter :verbose to toggle disassembly output"
       repl env verbose

run env _ (Cmd Quit) = return ()

run env verbose (Cmd Verbose) = repl env (Prelude.not verbose)

run env verbose stmt@(Assign var exp) =
    do (i, ins) <- eval' env stmt
       when verbose (mapM_ (putStrLn . showIntel) ins)
       repl env verbose

run env verbose stmt@(Print exp) =
    do (i, ins) <- eval' env stmt
       putStrLn (show i)
       when verbose (mapM_ (putStrLn . showIntel) ins)
       repl env verbose

-- Function for compiling and executing statements.
eval' :: Ptr Int32 -> Stmt -> IO (Int32, [Instruction])
eval' env e = do (_, Right v) <- runCodeGen (compileAndRun e) env ()
                 return v

compileAndRun :: Stmt -> CodeGen (Ptr Int32) s (Int32, [Instruction])
compileAndRun (Assign c exp) =
    do entryCode
       compileExp exp
       env <- getEnv
       mov (variableAddress env c) eax
       exitCode
       d <- disassemble
       callAsVoid 0
       return (0, d)
compileAndRun (Print exp) =
    do entryCode
       compileExp exp
       exitCode
       d <- disassemble
       r <- callAsWord32 0
       return (fromIntegral r, d)

compileExp :: Exp -> CodeGen (Ptr Int32) s ()
compileExp (Add e1 e2) = compileBinOp e1 e2 (add eax (Ind esp))
compileExp (Sub e1 e2) = compileBinOp e1 e2 (sub eax (Ind esp))
compileExp (Mul e1 e2) = compileBinOp e1 e2 (imul InPlace eax (Ind esp))
compileExp (Div e1 e2) = compileBinOp e1 e2 (cdq >> idiv (Ind esp))
compileExp (Lit i) = mov eax ((fromIntegral i) :: Word32)
compileExp (Var c) = do env <- getEnv
                        mov eax (variableAddress env c)

compileBinOp :: Exp -> Exp -> CodeGen (Ptr Int32) s a -> CodeGen (Ptr Int32) s ()
compileBinOp e1 e2 op = do compileExp e2
                           push eax
                           compileExp e1
                           op
                           add esp (4 :: Word32)

entryCode :: CodeGen e s ()
entryCode = do push ebp
               mov ebp esp

exitCode :: CodeGen e s ()
exitCode = do mov esp ebp
              pop ebp
              ret

variableAddress :: Ptr Int32 -> Char -> Addr
variableAddress env c =
    let ofs = fromEnum c - fromEnum 'a'
        env' = advancePtr env ofs
    in Addr (fromIntegral (ptrToWordPtr env'))
