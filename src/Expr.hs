module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail', satisfy, success, symbol)
import           Data.Char   (digitToInt, isDigit)

import           Control.Applicative

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
uberExpr [] elemP _                      = elemP 
uberExpr ((p, assoc):rest) elemP makeAST = case assoc of
  LeftAssoc -> do
      (first, rest) <- fmap (,) uberExpr' <*> (many $ fmap (,) p <*> uberExpr')
      return (foldl f first rest) where
        f l (op, r) = makeAST op l r
  RightAssoc -> do
      (rest, last) <- fmap (,) (many $ fmap (,) uberExpr' <*> p) <*> uberExpr'
      return (foldr f last rest) where
        f (l, op) r = makeAST op l r
  NoAssoc -> do
      l  <- uberExpr'
      op <- p
      r  <- uberExpr'
      return (makeAST op l r)
    <|> uberExpr'
  where
    uberExpr' = uberExpr rest elemP makeAST

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr lst elemP makeAST where
  lst      = [(parse '+' <|> parse '-', LeftAssoc), (parse '*' <|> parse '/', LeftAssoc), (parse '^', RightAssoc)]
  elemP    = (Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')')
  makeAST  = BinOp 
  parse op = symbol op >>= toOperator

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

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = success Plus
toOperator '*' = success Mult
toOperator '-' = success Minus
toOperator '/' = success Div
toOperator '^' = success Pow
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