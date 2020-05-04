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
					  (uncurry FunctionCall) <$> parseFunctionCall <|>
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

parseFunctionCall :: Parser String String (String, [AST])
parseFunctionCall = let
  lbr = parseWS <* symbol '(' <* parseWS
  rbr = parseWS <* symbol ')' <* parseWS
  in do
    name <- parseIdent
    args <- lbr *> (sepBy1 (parseWS <* symbol ',' <* parseWS) parseExpr <* rbr)
	    <|> lbr *> rbr *> (pure [])
    return (name, args)
    
    

parseWS :: Parser String String String
parseWS = many $ satisfy isSpace

opParser :: String -> Parser String String Operator
opParser x = (parseWS *> word x <* parseWS) >>= toOperator

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
