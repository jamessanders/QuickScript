import Parser2
import JS
import Text.Parsec
import Text.Parsec.String
import System.Environment

main = do
  [path] <- getArgs
  parsed <- parseFromFile (manyTill statement eof) path
  case parsed of 
    (Left e) -> print e
    p@(Right _) ->  putStrLn (evalJS parsed)