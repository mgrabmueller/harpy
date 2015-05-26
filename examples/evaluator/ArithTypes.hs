module ArithTypes where

import Foreign

data Stmt = Assign Char Exp
          | Print Exp
          | Cmd Cmd
          deriving (Show)

data Cmd = Help
         | Quit
         | Verbose
         deriving (Show)

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
         | Div Exp Exp
         | Lit Int32
         | Var Char
         deriving (Show)

