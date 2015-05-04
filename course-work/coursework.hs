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
    file1 <- readFile (args !! 1)
    file2 <- readFile (args !! 2)
  
    let command = (args !! 0)

    if command == "first"
      then print $ distanceMax (fileParser file1) (fileParser file2)
      else if command == "second"
        then threeTuplePrinter $ minimumFinder (fileParser file1) (fileParser file2)
        else if command == "third"
          then threeTuplePrinter $ minimumFinderMethod3 (fileParser file1) (fileParser file2)
          else
            putStrLn "Not implemented"

handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The input file doesn't exist!"
    | otherwise = putStrLn "Something whent wrong! Please check your params and try again!"

-- Take file content and turn into parsed data (sentences in arrays containing words in array)
-- We want to strip commas and dots, then group by separators, and filter them out in the end
-- The following section contains helpers for this.
fileParser :: String -> [[String]]
fileParser fileContent = toWords (toSentences (filterControls fileContent))

filterControls :: String -> String
filterControls input = filter (not . isControl) input

toWords :: [String] -> [[String]]
toWords input = map words $ input

toSentences :: String -> [String]
toSentences input = filter (not . any isSeparator2) . groupBy ((==) `on` isSeparator2) $ map toLower (stripCommas input)

stripCommas :: String -> String
stripCommas input = filter (not . isCommaDot) input

isCommaDot :: Char -> Bool
isCommaDot ch
  | ch == ',' = True
  | ch == ':' = True
  | otherwise = False

isSeparator2 :: Char -> Bool
isSeparator2 ch
    | ch == '.' = True
    | ch == ';' = True
    | ch == '!' = True
    | ch == '?' = True
    | otherwise = False

-- Function to calculate distance with first method
distanceMax :: [[String]] -> [[String]] -> Int
distanceMax xs ys = sum [sentenceDistance xss yss | xss <- xs, yss <- ys]

sentenceDistance :: [String] -> [String] -> Int
sentenceDistance xs ys = length $ (xs \\ ys) ++ (ys \\ xs)

-- Second method calculation. Finds the first sentence pairs with the smallest distance.
-- When the group of sentece pairs with the smallest distance is found, general helper functions
-- are used to print them according to the specification.
--
-- The output is a list of three tuples (Distance, first sentence, second sentence), which contain
-- sentence pairs and their distances (the smallest possible pairs). Execution is inspired by Learn
-- you a Haskell stock example.
minimumFinder :: [[String]] -> [[String]] -> [(Int, String, String)]
minimumFinder xs ys = head (dropWhile (\x -> (tupleSum x) > (minSum xs ys)) (candidateLists xs ys))

-- Returns all possible combinations of sentence pairs, from this list, the combination with the
-- smallest sum is calculated
candidateLists :: [[String]] -> [[String]] -> [[(Int, String, String)]]
candidateLists xs ys = [[(sentenceDistance xss (ys !! a), unwords xss, unwords (ys !! a)) | xss <- xs] | a <- [0..(length ys - 1)]]

-- Same as the above one, but the minimum possible sum and candidatelist is calculated differently
minimumFinderMethod3 :: [[String]] -> [[String]] -> [(Int, String, String)]
minimumFinderMethod3 xs ys = head (dropWhile (\x -> (tupleSum x) > (minSumMethod3 xs ys)) (candidateListsMethod3 xs ys))

-- Same as the above candidateLists function used for method 2, except calculation is a bit different.
-- Handling of list comprehension is split in to smaller functions to improve readability.
candidateListsMethod3 :: [[String]] -> [[String]] -> [[(Int,String,String)]]
candidateListsMethod3 xs ys = [tupleListCreator i j xs ys | i <- [0..(length xs - 1)], j <- [0..(length ys - 1)]]

tupleListCreator :: Int -> Int -> [[String]] -> [[String]] -> [(Int,String,String)]
tupleListCreator i j xs ys = (zipWith distanceCalculator (fst (splitAt i xs)) (fst (splitAt j ys))) ++ (zipWith distanceCalculator (snd (splitAt i xs)) (snd (splitAt j ys)))

distanceCalculator :: [String] -> [String] -> (Int,String,String)
distanceCalculator xs ys = (sentenceDistance xs ys, unwords xs, unwords ys)

-- Calculate total distance of sentences for the "candidate list"
tupleSum :: [(Int,String,String)] -> Int
tupleSum xs = sum [a | (a,b,c) <- xs]

-- Calculates the smallest sum of all possible candidate lists
minSum :: [[String]] -> [[String]] -> Int
minSum xs ys = minimum $ map (\x -> tupleSum x) (candidateLists xs ys)

minSumMethod3 :: [[String]] -> [[String]] -> Int
minSumMethod3 xs ys = minimum $ map (\x -> ((remainingLength x xs ys) + tupleSum x)) (candidateListsMethod3 xs ys)

-- Calculate the length of the remaining sentences
remainingLength :: [(Int,String,String)] -> [[String]] -> [[String]] -> Int
remainingLength x y z = sum $ map length $ (y ++ z) \\ (tupleToSentence x)

tupleToSentence :: [(Int,String,String)] -> [[String]]
tupleToSentence sents = (map (\x -> words (threeTupleSnd x)) sents) ++ (map (\x -> words (threeTupleThird x)) sents)

-- Helper functions for printing a three-tuple in the way stated in the course work
-- specification. And other helpers for handling 3-tuples
tupleToString :: (Int, String, String) -> String
tupleToString (a, b, c) = b ++ " & " ++ c 

threeTuplePrinter :: [(Int, String, String)] -> IO ()
threeTuplePrinter tupleList = do
                print $ tupleSum tupleList
                putStrLn . unlines . map tupleToString $ tupleList

threeTupleSnd :: (a, b, c) -> b
threeTupleSnd (_, b, _) = b

threeTupleThird :: (a, b, c) -> c
threeTupleThird (_, _, c) = c

