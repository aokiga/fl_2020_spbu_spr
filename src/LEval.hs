module LEval where

import LLang (Program (..), Configuration (..), parseProg, eval, Function (..))
import Combinators (Result (..), InputStream (..), runParser)
import qualified Data.Map    as Map

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program funcs main) input = eval main conf
    where conf = Conf Map.empty input [] (Map.fromList (zip (map name funcs) funcs))

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg code input =  case (runParser parseProg code) of
    Success (InputStream _ _) res -> evalProg res input 
    Failure _ -> Nothing