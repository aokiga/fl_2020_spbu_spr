module Expr where

import           AST         (AST (..), Operator (..))
import           Combinators (Parser (..), Result (..), elem', fail', satisfy, success, symbol, matchString)
import           Data.Char   (digitToInt, isDigit, isLetter)

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
  lst = [
      (parse "||", RightAssoc),
      (parse "&&", RightAssoc),
      (parse "<=" <|> parse "<" <|> parse ">=" <|> parse ">" <|> parse "==" <|> parse "/=", NoAssoc),
      (parse "+" <|> parse "-", LeftAssoc),
      (parse "*" <|> parse "/", LeftAssoc),
      (parse "^", RightAssoc)
    ]
  elemP    = (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
  makeAST  = BinOp
  parse op = matchString op >>= toOperator

-- Парсер для целых чисел
parseNum = foldl f 0 <$> go
  where
    go = some (satisfy isDigit) <|> (\x y -> y ++ x) <$> many (symbol '-') <*> some (satisfy isDigit)
    f acc '-' = -acc
    f acc d   = 10 * acc + digitToInt d

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLetter <|> symbol '_')) <*> many (satisfy isLetter <|> satisfy isDigit <|> symbol '_')

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = (parseOp' operators) >>= toOperator
  where
    parseOp' []     = matchString "" 
    parseOp' (s:xs) = (matchString s) <|> (parseOp' xs)
    operators       = ["+", "-", "*", "/", "^", "==", "/=", "<=", "<", ">=", ">", "&&", "||"]

-- Преобразование символов операторов в операторы
toOperator :: String -> Parser String String Operator
toOperator "+"  = success Plus
toOperator "*"  = success Mult
toOperator "-"  = success Minus
toOperator "/"  = success Div
toOperator "^"  = success Pow
toOperator "==" = success Equal
toOperator "/=" = success Nequal
toOperator ">=" = success Ge
toOperator ">"  = success Gt
toOperator "<=" = success Le
toOperator "<"  = success Lt
toOperator "&&" = success And
toOperator "||" = success Or
toOperator _    = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

notZero :: Int -> Bool
notZero 0 = False
notZero _ = True  

compute :: AST -> Int
compute (Num x)            = x
compute (BinOp Plus x y)   = compute x + compute y
compute (BinOp Mult x y)   = compute x * compute y
compute (BinOp Minus x y)  = compute x - compute y
compute (BinOp Div x y)    = compute x `div` compute y
compute (BinOp Pow x y)    = compute x ^ compute y
compute (BinOp Equal x y)  = fromEnum $ compute x == compute y
compute (BinOp Nequal x y) = fromEnum $ compute x /= compute y
compute (BinOp Gt x y)     = fromEnum $ compute x > compute y
compute (BinOp Ge x y)     = fromEnum $ compute x >= compute y
compute (BinOp Lt x y)     = fromEnum $ compute x < compute y
compute (BinOp Le x y)     = fromEnum $ compute x <= compute y
compute (BinOp And x y)    = fromEnum $ notZero (compute x) && notZero (compute y)
compute (BinOp Or x y)     = fromEnum $ notZero (compute x) || notZero (compute y)