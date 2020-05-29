module PLang where

import Combinators (Result (..), Parser (..), symbol, matchString, success, runParser, satisfy)
import Control.Applicative
import Data.List (elemIndex, intercalate)
import Text.Printf (printf)
import Data.Char (isUpper, isLower, isDigit, isLetter)

type Var = String
type Ident = String

data AST = Var String
         | Atom String [AST]
         | Relation String [AST] [AST]
         deriving (Eq)

data Program = Program { relations :: [AST], target :: [AST] }

parseBody :: Parser String String [AST]
parseBody = ((:) <$> (parseAtom) <*> many(matchString "," *> parseAtom))

parseIdent :: Parser String String String
parseIdent = ((:) <$> (satisfy isLower)) <*> many (satisfy isLetter <|> satisfy isDigit)

parseVar' :: Parser String String String
parseVar' = ((:) <$> (satisfy isUpper)) <*> many (satisfy isLetter <|> satisfy isDigit)

parseVar :: Parser String String AST
parseVar = do
    s <- parseVar'
    return $ Var s

parseAtom :: Parser String String AST
parseAtom = do
    name <- parseIdent
    args <- ((matchString "(" *> parseArgs) <* matchString ")") <|> (pure [])
    return $ Atom name args

parseArg :: Parser String String AST
parseArg = parseVar <|> parseAtom

parseArgs :: Parser String String [AST]
parseArgs = ((:) <$> (parseArg) <*> many(matchString "," *> parseArg))

parseRelations :: Parser String String AST
parseRelations = do
    name <- parseIdent
    args <- ((matchString "(" *> parseArgs) <* matchString ")") <|> (pure [])
    body <- (matchString ":-" *> parseBody) <|> (pure [])
    matchString "."
    return $ Relation name args body

parseTarget :: Parser String String [AST]
parseTarget = do
    matchString "?-"
    body <- parseBody <|> (pure [])
    matchString "."
    return body

modifyInput :: String -> String
modifyInput "" = ""
modifyInput (c : rest)
    | (c == ' ' || c == '\n') = modifyInput rest         
    | otherwise               = (c : modifyInput rest)

parseProg :: Parser String String Program
parseProg = Parser $ \input -> runParser' parseProg' (fmap modifyInput input) where
  parseProg' = do
    rel <- (many $ parseRelations)
    target <- parseTarget
    return $ Program rel target 

instance Show Program where
  show (Program rel targ) =
    printf "%s\n\n\ngoal: [\n%s\n]" (intercalate "\n\n" $ map show rel) (intercalate "\n\n" $ map (showAST 1) targ)

instance Eq Program where
  a == b = (show a) == (show b)

showAST :: Int -> AST -> String
showAST n ast = case ast of
    (Var name) -> 
      shift $ printf "%s" name
    (Atom name args) ->
      shift $ printf "%s\n%sargs: (\n%s\n%s)" name (shift "")  (intercalate "\n" $ map (showAST (n + 1)) args) (shift "") 
    (Relation name args atoms) -> 
      shift $ printf "%s\n%sargs: (\n%s\n%s)\n%sbody: [\n%s\n%s]" name (shift "")  (intercalate "\n" $ map (showAST (n + 1)) args) (shift "") (shift "") (intercalate "\n" $ map (showAST (n + 1)) atoms) (shift "")
  where 
    shift = identation n

instance Show AST where
  show = showAST 0

ident = (+1)
identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
