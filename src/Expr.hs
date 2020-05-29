module Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit, isLetter, isSpace)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary


uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> Parser e i ast
uberExpr ops term binAst = foldr f term ops where
  opFoldl a (op, b) = binAst op a b
  opFoldr (a, op) b = binAst op a b
  f (op, Binary NoAssoc) expr    = (\a op b -> binAst op a b) <$> expr <*> op <*> expr <|> expr
  f (op, Binary LeftAssoc) expr  = ((uncurry $ foldl opFoldl) <$> sepBy1l op expr) <|> expr
  f (op, Binary RightAssoc) expr = ((uncurry. flip $ foldr opFoldr) <$> sepBy1r op expr) <|> expr

-- Парсер для выражений над +, *
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0 и идентификаторами.
-- В строке могут быть скобки и пробелы

parseExpr :: Parser String String AST
parseExpr = parseWS *> (uberExpr [
                      (opParser "+", Binary LeftAssoc),
                      (opParser "*", Binary LeftAssoc)]
                      (Num <$> parseNum <|> 
                       symbol '(' *> parseExpr <* symbol ')' <|>
                       Ident <$> parseIdent)
                      staticBinOp) <* parseWS

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = (:) <$> (pltr <|> p_) <*> many (pltr <|> p_ <|> pdgt)
  where 
    p_   = symbol '_'
    pltr = satisfy isLetter
    pdgt = satisfy isDigit

parseWS :: Parser String String String
parseWS = many $ satisfy isSpace

opParser :: String -> Parser String String Operator
opParser x = (parseWS *> word x <* parseWS) >>= toOperator

-- Преобразование знаков операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = pure Plus
toOperator "*"  = pure Mult
toOperator _    = fail' "Failed toOperator"

staticPlus :: AST -> AST -> AST
staticPlus (Num 0) x = x
staticPlus x (Num 0) = x
staticPlus (Num x) (Num y) = Num (x + y)
staticPlus (Num x) (BinOp Plus (Num y) z) = BinOp Plus (Num (x + y)) z
staticPlus (BinOp Plus (Num y) z) (Num x) = BinOp Plus (Num (x + y)) z
staticPlus (BinOp Plus (Num x) y) (BinOp Plus (Num z) t) = BinOp Plus (Num (x + z)) (BinOp Plus y t)
staticPlus (BinOp Plus (Num x) y) z = BinOp Plus (Num x) (staticPlus y z)
staticPlus z (BinOp Plus (Num x) y) = BinOp Plus (Num x) (staticPlus y z)
staticPlus y (Num x) = BinOp Plus (Num x) y
staticPlus a b = BinOp Plus a b

staticMult :: AST -> AST -> AST
staticMult (Num 0) _ = Num 0
staticMult _ (Num 0) = Num 0
staticMult (Num 1) x = x
staticMult x (Num 1) = x
staticMult (Num x) (Num y) = Num (x * y)
staticMult (Num x) (BinOp Mult (Num y) z) = BinOp Mult (Num (x * y)) z
staticMult (BinOp Mult (Num y) z) (Num x) = BinOp Mult (Num (x * y)) z
staticMult (BinOp Mult (Num x) y) (BinOp Mult (Num z) t) = BinOp Mult (Num (x * z)) (BinOp Mult y t)
staticMult (BinOp Mult (Num x) y) z = BinOp Mult (Num x) (staticMult y z)
staticMult z (BinOp Mult (Num x) y) = BinOp Mult (Num x) (staticMult y z)
staticMult y (Num x) = BinOp Mult (Num x) y
staticMult a b = BinOp Mult a b

staticBinOp :: Operator -> AST -> AST -> AST
staticBinOp Plus = staticPlus
staticBinOp Mult = staticMult