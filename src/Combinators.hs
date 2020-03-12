module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Result error input) where
  fmap f (Success inp res) = Success inp (f res)
  fmap f (Failure e) = Failure e

instance Functor (Parser error input) where
  fmap f (Parser p) = Parser $ (fmap f) . p

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x

  fp <*> p = Parser $ \input ->
    case runParser fp input of
      Success input' f -> runParser (f <$> p) input'
      Failure e        -> Failure e                      

instance Monad (Parser error input) where
  return = pure

  p >>= f = Parser $ \input ->
    case runParser p input of
      Success i r -> runParser (f r) i
      Failure e   -> Failure e

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ const (Failure mempty)

  Parser f <|> Parser g = Parser h where
    h input = case f input of
      Failure _ -> g input
      res -> res

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> many (sep *> elem)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    []           -> Failure $ "Empty string"
    (x:xs)       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = pure ()

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure