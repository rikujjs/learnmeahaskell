import System.Environment
import Data.Char
import System.IO
import System.IO.Error
import Control.Exception

import Data.List
import Data.Function

main = wordCounter `catch` handler

wordCounter = do
    args <- getArgs
    file1 <- readFile (args !! 1) -- String
    file2 <- readFile (args !! 2)
  

    let command = (args !! 0)

    if command == "first"
      then print $ distanceMax (fileParser file1) (fileParser file2)
      else if command == "second"
        then threeTuplePrinter $ minimumFilter (fileParser file1) (fileParser file2)
        else if command == "third"
          then putStrLn "third"
          else
            putStrLn "Not implemented"

-- Take file content and turn into parsed data (sentences in arrays containing words in array)
fileParser :: String -> [[String]]
fileParser fileContent = toWords (toSentences fileContent)


checkParam :: String -> String
checkParam param
  | param == "first" = "Operation first"
  | param == "second" = "Operation second"
  | param == "third" = "Operation third"
  | otherwise =  "Not implemented"

handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The input file doesn't exist!"
    | otherwise = ioError e

distanceMax :: [[String]] -> [[String]] -> Int
distanceMax xs ys = sum [sentenceDist xss yss | xss <- xs, yss <- ys]

distanceMin :: [[String]] -> [[String]] -> Int
distanceMin xs ys = minimum $ map (\x -> tupleSum x) (candidateSents xs ys)

tupleSum :: [(Int,String,String)] -> Int
tupleSum xs = sum [a | (a,b,c) <- xs]

minSum :: [[String]] -> [[String]] -> Int
minSum xs ys = minimum $ map (\x -> tupleSum x) (candidateSents xs ys)

-- Returns the first (in order) solution that has the minimum sum of differences!
minimumFilter :: [[String]] -> [[String]] -> [(Int, String, String)]
minimumFilter xs ys = head (dropWhile (\x -> (tupleSum x) > (minSum xs ys)) (candidateSents xs ys))

tupleToString :: (Int, String, String) -> String
tupleToString (a, b, c) = b ++ " & " ++ c 

threeTuplePrinter :: [(Int, String, String)] -> IO ()
threeTuplePrinter tupleList = putStrLn . unlines . map tupleToString $ tupleList

candidateSents :: [[String]] -> [[String]] -> [[(Int, String, String)]]
candidateSents xs ys = [[(sentenceDist xss (ys !! a), unwords xss, unwords (ys !! a)) | xss <- xs] | a <- [0..(length ys - 1)]]

sentenceDist :: [String] -> [String] -> Int
sentenceDist xs ys = length $ (xs \\ ys) `union` (ys \\ xs)

toWords :: [String] -> [[String]]
toWords input = map words $ input

toSentences :: String -> [String]
toSentences input = filter (not . any isSprtr) . groupBy ((==) `on` isSprtr) $ map toLower (stripCommas input)

stripCommas :: String -> String
stripCommas input = filter (not . isCommaOrDdot) input

isCommaOrDdot :: Char -> Bool
isCommaOrDdot ch
  | ch == ',' = True
  | ch == ':' = True
  | otherwise = False

isSprtr :: Char -> Bool
isSprtr ch
    | ch == '.' = True
    | ch == ';' = True
    | ch == '!' = True
    | ch == '?' = True
    | otherwise = False