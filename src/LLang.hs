module LLang where

import AST (AST (..), Operator (..))
import Combinators
import Expr
import Control.Applicative
import Data.Char (isSpace)
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)


stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

-- Парсер для пробелов

ws :: Parser String String String
ws = many $ satisfy isSpace

-- Парсеры для команд
  
parseIf :: Parser String String LAst
parseIf = let 
  parseElse = do
    prefix "else"
    satisfy isSpace
    els <- parseL
    return els
  in do 
    prefix "if" <* ws
    cond <- symbol '(' *> parseExpr <* symbol ')'
    thn <- parseL
    els <- parseElse <|> pure (Seq [])
    return $ If cond thn els

parseWhile :: Parser String String LAst
parseWhile = do
  prefix "while" <* ws
  cond <- symbol '(' *> parseExpr <* symbol ')'
  body <- parseL
  return $ While cond body

parseAssign :: Parser String String LAst
parseAssign = do
  var <- parseIdent
  ws *> symbol '='
  expr <- parseExpr
  return $ Assign var expr
  
parseRead :: Parser String String LAst
parseRead = do
  prefix "read" <* ws
  symbol '(' <* ws
  var <- parseIdent
  ws <* symbol ')'
  return $ Read var
  
parseWrite :: Parser String String LAst
parseWrite = do
  prefix "print" <* ws
  symbol '('
  expr <- parseExpr
  ws <* symbol ')'
  return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
  symbol '{'
  cmds <- many $ parseL <* symbol ';'
  ws <* symbol '}'
  return $ Seq cmds

parseL :: Parser String String LAst
parseL = ws *> (parseIf <|> parseWhile <|> 
                parseAssign <|> parseRead <|>
                parseWrite <|> parseSeq) <* ws


initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"
