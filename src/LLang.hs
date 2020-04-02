module LLang where

import AST (AST (..), Operator (..))
import Combinators (Result (..), Parser (..), symbol, matchString)
import Expr (parseExpr, parseIdent)
import Control.Applicative
import Data.List (elemIndex)

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

stmt :: LAst
stmt =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

parseCondition :: Parser String String AST
parseCondition = do
    parseSpaces
    matchString "("
    parseSpaces
    expr <- parseExpr
    parseSpace
    matchString ")"
    return expr

parseIf :: Parser String String LAst
parseIf = do
    matchString "if"
    condition <- parseCondition
    parseSpace
    block1 <- parseSeq
    parseSpace
    matchString "else"
    parseSpace
    block2 <- parseSeq
    return $ If condition block1 block2

parseWhile :: Parser String String LAst
parseWhile = do
    matchString "while"
    condition <- parseCondition
    parseSpace
    block <- parseSeq
    return $ While condition block

parseAssign :: Parser String String LAst
parseAssign = do
    matchString "va"
    parseSpace
    name <- parseIdent
    parseSpace
    expr <- parseCondition
    return $ Assign name expr

parseRead :: Parser String String LAst
parseRead = do
    matchString "ead"
    parseSpace
    name <- parseIdent
    return $ Read name

parseWrite :: Parser String String LAst
parseWrite = do
    matchString "pint"
    expr <- parseCondition
    return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
    matchString "{"
    parseSpace
    commands <- many $ parseCommand  <* parseSpace <* matchString ";" <* parseSpace
    matchString "}"
    return $ Seq commands

parseCommand :: Parser String String LAst
parseCommand = parseIf <|> parseWhile <|> parseAssign <|> parseRead <|> parseRead <|> parseSeq

invSymbols :: String
invSymbols = " \t\n\v\f\r"

modifyInput :: String -> String
modifyInput "" = ""
modifyInput (c : rest)
  | (c == 'R' || c == 'r')            = modifyInput rest                   
  | elemIndex c invSymbols == Nothing = (c : modifyInput rest)   
  | otherwise                         = (' ' : modifyInput rest)
      
parseL :: Parser String String LAst
parseL = Parser $ \input -> runParser parseL' (modifyInput input) where
  parseL' = do
    parseSpaces
    result <- parseSeq
    parseSpaces
    return result 

parseSpace :: Parser String String String
parseSpace = matchString " "

parseSpaces :: Parser String String String
parseSpaces = many $ symbol ' '