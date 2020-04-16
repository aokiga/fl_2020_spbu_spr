module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators (Result (..), Parser (..), symbol, matchString, success, runParser)
import Expr (parseExpr, parseIdent, evalExpr)
import Control.Applicative
import Data.List (elemIndex, intercalate)
import Text.Printf (printf)

import qualified Data.Map    as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

instance Eq Program where
  a == b = (show a) == (show b)

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

instance Eq Function where
  a == b = (show a) == (show b)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  | Return { expr :: Expr }
  deriving (Eq)

parseCondition :: Parser String String AST
parseCondition = do
    parseSpaces
    parseString "("
    parseSpaces
    expr <- parseExpr
    parseSpaces
    parseString ")"
    return expr

parseIf :: Parser String String LAst
parseIf = do
    parseString "if"
    parseSpaces
    condition <- parseCondition
    parseSpaces
    block1 <- parseSeq
    parseSpaces
    parseString "else"
    parseSpaces
    block2 <- parseSeq
    return $ If condition block1 block2

parseWhile :: Parser String String LAst
parseWhile = do
    parseString "while"
    parseSpaces
    condition <- parseCondition
    parseSpaces
    block <- parseSeq
    return $ While condition block

parseAssign :: Parser String String LAst
parseAssign = do
    parseString "va"
    parseSpace
    parseSpaces
    name <- parseIdent
    parseSpaces
    expr <- parseCondition
    return $ Assign name expr

parseRead :: Parser String String LAst
parseRead = do
    parseString "ead"
    parseSpace
    parseSpaces
    name <- parseIdent
    return $ Read name

parseWrite :: Parser String String LAst
parseWrite = do
    parseString "pint"
    parseSpaces
    expr <- parseCondition
    return $ Write expr

parseSeq :: Parser String String LAst
parseSeq = do
    parseString "{"
    parseSpaces
    commands <- many $ parseCommand <* parseSpaces <* parseString ";" <* parseSpaces
    parseString "}"
    return $ Seq commands

parseReturn :: Parser String String LAst
parseReturn = do
    parseString "etun"
    parseSpaces
    expr <- parseCondition
    return $ Return expr

parseCommand :: Parser String String LAst
parseCommand = parseIf <|> parseWhile <|> parseAssign <|> parseRead <|> parseWrite <|> parseReturn <|> parseSeq

parseArgs :: Parser String String [Var]
parseArgs = ((:) <$> (parseIdent) <*> many(parseSpaces *> parseString "," *> parseSpaces *> parseIdent)) <|> (pure [])

parseDef :: Parser String String Function
parseDef = do
    parseString "fun"
    parseSpace
    parseSpaces
    name <- parseIdent
    parseSpaces
    parseString "("
    parseSpaces
    args <- parseArgs
    parseSpaces
    parseString ")"
    parseSpaces
    body <- parseSeq
    return $ Function name args body

parseProg :: Parser String String Program
parseProg = Parser $ \input -> runParser' parseProg' (fmap modifyInput input) where
  parseProg' = do
    funcs <- (many $ parseSpaces *> parseDef)
    main <- parseL
    return $ Program funcs main

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration 
eval (If cond bl1 bl2) config@(Conf subst input output) = do
    resCond <- evalExpr subst cond
    case resCond of
      0 -> eval bl2 config
      _ -> eval bl1 config

eval while@(While cond bl) config@(Conf subst input output) = do
    resCond <- evalExpr subst cond
    case resCond of
      0 -> return config
      _ -> do
        config' <- eval bl config
        eval while config'

eval (Assign name expr) (Conf subst input output) = do
    resExpr <- evalExpr subst expr
    return $ Conf (Map.insert name resExpr subst) input output

eval (Read name) (Conf subst input output) =
  case input of
    (x:rest) -> return $ Conf (Map.insert name x subst) rest output
    _      -> Nothing

eval (Write expr) (Conf subst input output) = do
    resExpr <- evalExpr subst expr
    return $ Conf subst input (resExpr:output)

eval (Seq instr) config =
  case instr of
    []     -> Just config
    (x:rest) -> do
      resInstr <- eval x config
      eval (Seq rest) resInstr

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (identation 1) $ lines $ show funBody)

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIdent = identation n in
        case t of
          If cond thn els -> makeIdent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) thn) (makeIdent "") (go (ident n) els)
          While cond body -> makeIdent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIdent "") (go (ident n) body)
          Assign var expr -> makeIdent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIdent $ printf "read %s" var
          Write expr      -> makeIdent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIdent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)

invSymbols :: String
invSymbols = " \t\n\v\f\r"

modifyInput :: String -> String
modifyInput "" = ""
modifyInput (c : rest)
  | (c == 'R' || c == 'r')            = modifyInput rest                   
  | elemIndex c invSymbols == Nothing = (c : modifyInput rest)   
  | otherwise                         = (' ' : modifyInput rest)

ident = (+1)

identation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id

parseL :: Parser String String LAst
parseL = do
    parseSpaces
    result <- parseSeq
    parseSpaces
    return result

parseString :: String -> Parser String String String
parseString = matchString

parseSpace :: Parser String String String
parseSpace = parseString " "

parseSpaces :: Parser String String String
parseSpaces = many $ symbol ' '
