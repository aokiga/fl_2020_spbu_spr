module Expr where

import           AST         (AST (..), Operator (..), Subst (..))
import           Combinators (Parser (..), Result (..), bind', elem', fail',
                              fmap', satisfy, some', success, symbol, matchString)
import           Data.Char   (digitToInt, isDigit, isLetter)

import qualified Data.Map as Map
import           Control.Applicative

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = error "evalExpr undefined"

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr [] elemP _ _ = elemP 
uberExpr ((p, assoc):rest) elemP makeAST makeUAST = case assoc of
  Binary LeftAssoc  -> do
      (first, rest) <- fmap (,) uberExpr' <*> (many $ fmap (,) p <*> uberExpr')
      return (foldl f first rest)
    <|> uberExpr'
        where f l (op, r) = makeAST op l r
  Binary RightAssoc -> do
      (rest, last) <- fmap (,) (many $ fmap (,) uberExpr' <*> p) <*> uberExpr'
      return (foldr f last rest)
    <|> uberExpr'
        where f (l, op) r = makeAST op l r
  Binary NoAssoc    -> do
      l  <- uberExpr'
      op <- p
      r  <- uberExpr'
      return (makeAST op l r)
    <|> uberExpr'
  Unary             -> fmap makeUAST p <*> uberExpr' <|> uberExpr'
  where
    uberExpr' = uberExpr rest elemP makeAST makeUAST


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки

parseExpr :: Parser String String AST
parseExpr = uberExpr lst elemP makeAST makeUAST where
  lst = [
      (parse "||", Binary RightAssoc),
      (parse "&&", Binary RightAssoc),
      (parse "!", Unary),
      (parse "<=" <|> parse "<" <|> parse ">=" <|> parse ">" <|> parse "==" <|> parse "/=", Binary NoAssoc),
      (parse "+" <|> parse "-", Binary LeftAssoc),
      (parse "*" <|> parse "/", Binary LeftAssoc),
      (parse "-", Unary),
      (parse "^", Binary RightAssoc)
    ]
  elemP    = (Num <$> parseNum <|> Ident <$> parseIdent <|> symbol '(' *> parseExpr <* symbol ')')
  makeAST  = BinOp
  makeUAST = UnaryOp
  parse op = matchString op >>= toOperator

-- Парсер для целых чисел
parseNum = foldl f 0 <$> go
  where
    go = some (satisfy isDigit)
    f acc d   = 10 * acc + digitToInt d

parseNegNum = foldl f 0 <$> go
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
    operators       = ["+", "-", "*", "/", "^", "==", "/=", "<=", "<", ">=", ">", "&&", "||", "!"]

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
toOperator "!"  = success Not
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
compute (UnaryOp Minus x)  = -(compute x)
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
compute (UnaryOp Not x)    = fromEnum $ not $ notZero $ compute x 