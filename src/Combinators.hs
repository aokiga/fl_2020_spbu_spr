module Combinators where

import Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Result error input) where
  fmap f (Success input result) = Success input (f result)
  fmap f (Failure error)        = Failure error

instance Functor (Parser error input) where
  fmap f (Parser p) = Parser runParser' where
    runParser' input = fmap f (p input)

instance Applicative (Parser error input) where
  pure result = Parser runParser' where
    runParser' input = Success input result

  (<*>) (Parser p1) (Parser p2) = Parser runParser' where 
    runParser' input = case p1 input of
      Success input' result -> fmap result (p2 input')
      Failure error         -> Failure error

instance Monad (Parser error input) where
  return = pure
  (>>=) (Parser p) k = Parser runParser' where
    runParser' input = case p input of
      Success input' result -> runParser (k result) input'
      Failure error         -> Failure error

instance Monoid error => Alternative (Parser error input) where
  empty = Parser runParser' where
    runParser' input = Failure mempty

  (<|>) (Parser p1) (Parser p2) = Parser runParser' where
    runParser' input = case p1 input of
      Success input' result -> Success input' result
      Failure error         -> p2 input

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = fmap (:) elem <*> many (sep *> elem)

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
    _            -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure