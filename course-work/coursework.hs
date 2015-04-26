import System.Environment
import Data.Char
import System.IO
import System.IO.Error
import Control.Exception

main = wordCounter `catch` handler

wordCounter = do
    args <- getArgs
    file1 <- readFile (args !! 1)
    file2 <- readFile (args !! 2)
    
    putStrLn (checkParam (args !! 0))

checkParam :: String -> String
checkParam param
  | param == "first" = "Operation first"
  | param == "second" = "Operation second"
  | param == "third" = "Operation third"
  | otherwise =  "Operation not recognized!"

handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The input file doesn't exist!"
    | otherwise = ioError e  
