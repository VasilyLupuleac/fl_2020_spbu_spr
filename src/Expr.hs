module Expr where

import           Control.Applicative (Alternative (..))
import           AST         (AST (..), Operator (..))
import           Combinators
import           Data.Char   (digitToInt, isDigit)

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr ops term ast = foldr f term ops where
  opFoldl a (op, b) = ast op a b
  opFoldr (a, op) b = ast op a b
  f (op, NoAssoc) expr    = (\a op b -> ast op a b) <$> expr <*> op <*> expr <|> expr
  f (op, LeftAssoc) expr  = (uncurry $ foldl opFoldl) <$> sepBy1l op expr
  f (op, RightAssoc) expr = (uncurry. flip $ foldr opFoldr) <$> sepBy1r op expr

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки

parseExpr :: Parser String String AST
parseExpr = uberExpr [(opParser '+' <|> opParser '-', LeftAssoc),
                      (opParser '*' <|> opParser '/', LeftAssoc),
                      (opParser '^', RightAssoc)]
                      (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')')
                      BinOp
                      

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseIdent :: Parser String String String
parseIdent = error "parseIdent undefined"

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

opParser :: Char -> Parser String String Operator
opParser x = symbol x >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = pure Plus
toOperator '*' = pure Mult
toOperator '-' = pure Minus
toOperator '/' = pure Div
toOperator '^' = pure Pow
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = compute x ^ compute y

