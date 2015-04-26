import System.Environment
import Data.List
import System.Random
import Control.Monad(when)

main = do
	args <- getArgs
	let seedNum = mkStdGen (read (head args))
	numberRandomizer seedNum

numberRandomizer :: StdGen -> IO ()
numberRandomizer seed = do
	let (randNumber, newGen) = randomR (1,6) seed :: (Int, StdGen)
	inputString <- getLine
	when (not $ inputString == "quit") $ do
		if inputString == "throw"
			then print randNumber
			else putStrLn "Couldn't understand input"
		numberRandomizer newGen
