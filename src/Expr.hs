module Expr where

import           AST         (AST (..), Operator (..), Subst)
import           Combinators
import           Data.Char   (digitToInt, isDigit, isLetter, isSpace)
import           Control.Applicative
import qualified Data.Map as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary


-- Соответствие между логическими и целочисленными значениями

fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1

toBool :: Int -> Bool
toBool 0 = False
toBool _ = True

-- Вычисление выражения

evalExpr :: Subst -> AST -> Maybe Int
evalExpr _ (Num x)                = Just x
evalExpr subst (Ident v)          = Map.lookup v subst
evalExpr subst (BinOp Plus x y)   = (+) <$> evalExpr subst x <*> evalExpr subst y
evalExpr subst (BinOp Mult x y)   = (*) <$> evalExpr subst x <*> evalExpr subst y
evalExpr subst (BinOp Minus x y)  = (-) <$> evalExpr subst x <*> evalExpr subst y
evalExpr subst (BinOp Div x y)    = (div) <$> evalExpr subst x <*> evalExpr subst y
evalExpr subst (BinOp Mod x y)    = (mod) <$> evalExpr subst x <*> evalExpr subst y
evalExpr subst (BinOp Gt x y)     = fromBool <$> ((>) <$> evalExpr subst x <*> evalExpr subst y)
evalExpr subst (BinOp Ge x y)     = fromBool <$> ((>=) <$> evalExpr subst x <*> evalExpr subst y)
evalExpr subst (BinOp Lt x y)     = fromBool <$> ((<) <$> evalExpr subst x <*> evalExpr subst y)
evalExpr subst (BinOp Le x y)     = fromBool <$> ((<=) <$> evalExpr subst x <*> evalExpr subst y)
evalExpr subst (BinOp Equal x y)  = fromBool <$> ((==) <$> evalExpr subst x <*> evalExpr subst y)
evalExpr subst (BinOp Nequal x y) = fromBool <$> ((/=) <$> evalExpr subst x <*> evalExpr subst y)
evalExpr subst (BinOp And x y)    = fromBool <$> ((&&) <$> (toBool <$> evalExpr subst x) <*> (toBool <$> evalExpr subst y))
evalExpr subst (BinOp Or x y)     = fromBool <$> ((&&) <$> (toBool <$> evalExpr subst x) <*> (toBool <$> evalExpr subst y))
evalExpr subst (UnaryOp Minus x)  = (0-) <$> evalExpr subst x
evalExpr subst (UnaryOp Not x)    = fromBool <$> ((==0) <$> evalExpr subst x)


uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr ops term binAst unAst = foldr f term ops where
  opFoldl a (op, b) = binAst op a b
  opFoldr (a, op) b = binAst op a b
  f (op, Unary) expr             = (\op a -> unAst op a) <$> op <*> expr <|> expr
  f (op, Binary NoAssoc) expr    = (\a op b -> binAst op a b) <$> expr <*> op <*> expr <|> expr
  f (op, Binary LeftAssoc) expr  = ((uncurry $ foldl opFoldl) <$> sepBy1l op expr) <|> expr
  f (op, Binary RightAssoc) expr = ((uncurry. flip $ foldr opFoldr) <$> sepBy1r op expr) <|> expr

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки

parseExpr :: Parser String String AST
parseExpr = parseWS *> uberExpr [(opParser "||", Binary RightAssoc),
                      (opParser "&&", Binary RightAssoc),
                      (opParser "!", Unary),
                      (opParser "==" <|> opParser "/=" <|>
                       opParser "<=" <|> opParser "<"  <|>
                       opParser ">=" <|> opParser ">", Binary NoAssoc),
                      (opParser "+" <|> opParser "-", Binary LeftAssoc),
                      (opParser "*" <|>
                       opParser "/" <|> opParser "%", Binary LeftAssoc),
                      (opParser "-", Unary),
                      (opParser "^", Binary RightAssoc)]
                      (Num <$> parseNum <|> 
                       symbol '(' *> parseExpr <* symbol ')' <|> 
                       Ident <$> parseIdent)
                      BinOp
                      UnaryOp <* parseWS

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
opParser x = (parseWS *> prefix x <* parseWS) >>= toOperator

-- Преобразование знаков операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = pure Plus
toOperator "*"  = pure Mult
toOperator "-"  = pure Minus
toOperator "/"  = pure Div
toOperator "%"  = pure Mod
toOperator "^"  = pure Pow
toOperator "==" = pure Equal
toOperator "/=" = pure Nequal
toOperator ">=" = pure Ge
toOperator ">"  = pure Gt
toOperator "<=" = pure Le
toOperator "<"  = pure Lt
toOperator "&&" = pure And
toOperator "||" = pure Or
toOperator "!"  = pure Not
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing



compute :: AST -> Int
compute (Num x)            = x
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Mod x y)    = compute x `mod` compute y
compute (BinOp Gt x y)     = fromBool $ compute x > compute y
compute (BinOp Ge x y)     = fromBool $ compute x >= compute y
compute (BinOp Lt x y)     = fromBool $ compute x < compute y
compute (BinOp Le x y)     = fromBool $ compute x <= compute y
compute (BinOp Equal x y)  = fromBool $ compute x == compute y
compute (BinOp Nequal x y) = fromBool $ compute x /= compute y
compute (BinOp And x y)    = fromBool $ (toBool $ compute x) && (toBool $ compute y)
compute (BinOp Or x y)     = fromBool $ (toBool $ compute x) || (toBool $ compute y)
compute (UnaryOp Minus x)  = 0 - compute x
compute (UnaryOp Not x)    = fromBool $ compute x == 0