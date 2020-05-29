module Main where
import Combinators (runParser, Result (..))
import Text.Printf (printf)
import PLang       (parseProg)

main :: IO ()
main = do
    input <- readFile "input.txt"
    writeFile "output.txt" (show $ runParser parseProg input)
