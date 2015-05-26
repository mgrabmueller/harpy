module ArithParser where

import Control.Monad

import ArithTypes

import Foreign

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr

lexer :: P.TokenParser ()
lexer  = P.makeTokenParser 
         (haskellStyle
         { reservedOpNames = ["*","/","+","-"]
         })

statement :: Parser Stmt
statement = do s <- statement'
               eof
               return s

statement' :: Parser Stmt
statement' = try((do [i] <- identifier
                     if i < 'a' || i > 'z'
                       then fail "character a-z expected"
                       else return ()
                     symbol ":="
                     e <- expr
                     return $ Assign i e) <?> "assignment")
            <|> liftM Cmd cmd
            <|> liftM Print expr

cmd :: Parser Cmd
cmd = try (do symbol ":help"
              return Help)
      <|> try (do symbol ":verbose"
                  return Verbose)
      <|> (do symbol ":quit"
              return Quit)

expr    :: Parser Exp
expr    = buildExpressionParser table factor
        <?> "expression"

table   = [[op "*" Mul AssocLeft, op "/" Div AssocLeft]
          ,[op "+" Add AssocLeft, op "-" Sub AssocLeft]
          ]
        where
          op s f assoc
             = Infix (do{ reservedOp s; return f} <?> "operator") assoc

factor  =   parens expr
        <|> liftM (Lit . fromInteger) natural
        <|> (do [i] <- identifier
                if i < 'a' || i > 'z'
                   then fail "character a-z expected"
                   else return ()
                return $ Var i)
        <?> "simple expression"

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

