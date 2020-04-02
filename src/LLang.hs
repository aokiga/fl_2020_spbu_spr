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
    parseSpaces
    matchString ")"
    return expr

parseIf :: Parser String String LAst
parseIf = do
    matchString "if"
    condition <- parseCondition
    parseSpaces
    block1 <- parseSeq
    parseSpaces
    matchString "else"
    parseSpaces
    block2 <- parseSeq
    return $ If condition block1 block2

parseWhile :: Parser String String LAst
parseWhile = do
    matchString "while"
    parseSpaces
    condition <- parseCondition
    parseSpaces
    block <- parseSeq
    return $ While condition block

parseAssign :: Parser String String LAst
parseAssign = do
    matchString "va"
    parseSpace
    parseSpaces
    name <- parseIdent
    parseSpace
    parseSpaces
    expr <- parseCondition
    return $ Assign name expr

parseRead :: Parser String String LAst
parseRead = do
    matchString "ead"
    parseSpace
    parseSpaces
    name <- parseIdent
    return $ Read name

parseWrite :: Parser String String LAst
parseWrite = do
    matchString "pint"
    parseSpace
    parseSpaces
    expr <- parseCondition
    return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
    matchString "{"
    parseSpaces
    commands <- many $ parseCommand  <* parseSpaces <* matchString ";" <* parseSpaces
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