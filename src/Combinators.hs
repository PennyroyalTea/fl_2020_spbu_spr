module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f (Parser p) = Parser $ \input ->
  	case p input of
    	Success i a -> Success i (f a)
    	Failure e   -> Failure e

instance Applicative (Parser error input) where
  pure x = Parser $ \input -> Success input x
  (Parser f) <*> (Parser x) = Parser $ \input -> 
  	case f input of
  		Failure e -> Failure e
  		Success i f -> case x i of
  						Success i' a -> Success i' (f a)
  						Failure e -> Failure e

instance Monad (Parser error input) where
  return = pure
  (Parser p) >>= f = Parser $ \input ->
  	case p input of
    	Success i r -> runParser (f r) i
    	Failure e   -> Failure e

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \_ -> Failure mempty

  (Parser p) <|> (Parser q) = Parser $ \input -> case p input of
                                Failure _ -> q input
                                x -> x 

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> (many $ sep *> elem)

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
epsilon = return ()

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: Monoid e => Parser e i a -> Parser e i [a]
some' p = do 
  a <- p
  as <- many' p
  return (a : as)

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: Monoid e => Parser e i a -> Parser e i [a]
many' p =
  some' p <|> return []