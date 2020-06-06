module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail',
                              satisfy, some', symbol)
import           Control.Monad
import           Control.Applicative
import           Data.Char   (digitToInt, isDigit, isLetter)

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
uberExpr operators pE build = foldr f pE operators where
	f (op, assoc) pE = case assoc of
		NoAssoc -> do 
			e <- pE
			((`build` e) <$> op <*> pE) <|> return e
		LeftAssoc -> do 
			e <- pE
			lst <- many ((,) <$> op <*> pE)
			return $ foldl (\e1 (op, e2) -> build op e1 e2) e lst
		RightAssoc -> do 
			(lst, e) <- (,) <$> (many ((,) <$> pE <*> op)) <*> pE
			return $ foldr (\(b, op) a -> build op b a) e lst

toParser c = parseStr c >>= toOperator

operators = [
	(toParser "+", LeftAssoc), 
	(toParser "*", LeftAssoc)]

-- Парсер для выражений над +, *
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = 
	uberExpr operators 
	((Num <$> parseNum) 
		<|> (Ident <$> parseIdent) 
		<|> (symbol '(' *> parseExpr <* symbol ')')) 
	BinOp

-- Парсер для целых чисел
parseNum' :: Parser String String Int
parseNum' = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some' (satisfy isDigit)


parseNum :: Parser String String Int
parseNum = Parser $ \input ->
  case input of 
    ('-':xs) -> case runParser parseNum xs of
      Success i r -> Success i (r * (-1))
      e -> e
    otherwise -> runParser parseNum' input

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLetter <|> symbol '_')) <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = foldr (\op lst -> ((parseStr op) >>= toOperator) <|> lst) empty ["+", "*"]

parseStr (x:xs) = do
  y <- symbol x
  ys <- parseStr xs
  return $ y:ys
parseStr [] = Parser $ \input -> Success input ""

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+" = return Plus
toOperator "*" = return Mult
toOperator _   = fail' "Failed toOperator"

print' :: AST -> String
print' (Num x) = show x
print' (Ident x) = x
print' (BinOp Mult l r) = printBrac l ++ show Mult ++ printBrac r where
	printBrac t@(BinOp Plus l r) = "(" ++ print' t ++ ")"
	printBrac t = print' t
print' (BinOp op l r) = print' l ++ show op ++ print' r

simplify :: String -> Maybe String
simplify = (fmap print') . simplify'

simplify' :: String -> Maybe AST
simplify' input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> AST -- optimize all levels
compute (BinOp op l r) = optimize (BinOp op (compute l) (compute r))
compute node = optimize node

optimize :: AST -> AST -- optimize 1 level
optimize (BinOp Plus (Num x) (Num y)) = Num $ x + y
optimize (BinOp Mult (Num x) (Num y)) = Num $ x * y
optimize (BinOp Plus (Num 0) y) = y
optimize (BinOp Plus x (Num 0)) = x
optimize (BinOp Mult (Num 1) y) = y
optimize (BinOp Mult x (Num 1)) = x
optimize (BinOp Mult (Num 0) _) = Num 0
optimize (BinOp Mult _ (Num 0)) = Num 0
optimize node = node