-- In the beggining call with value 0 to start off the recursion.
main = do
    numberAdder 0

numberAdder :: Integer -> IO ()
numberAdder number = do
    inputString <- getLine
    if null inputString
        then print number
        else do
            let inputNum = read inputString
            numberAdder $ inputNum + number