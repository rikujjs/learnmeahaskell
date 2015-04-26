import Data.Char

-- Encodes given string
encodeStr :: String -> Int -> Int -> String
encodeStr msg k n = caesar n (cipher msg k)

-- Decodes given string
decodeStr :: String -> Int -> Int -> String
decodeStr msg k n = deCaesar n (cipher msg k)

-- Scramble substrings
cipher :: String -> Int -> String
cipher xs k
    | length xs <= k = reverse xs
    | otherwise = reverse (take k xs) ++ cipher (drop k xs) k

-- the Caesar encoding method
caesar :: Int -> String -> String
caesar shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

deCaesar :: Int -> String -> String
deCaesar shift msg = caesar (negate shift) msg


-- EX3


data ThreeTuple a  = ThreeTuple { x :: a
                          , y :: a
                          , z :: a
                          , d :: a
                          } deriving (Show)


--1
tupleSum :: (Floating a, Ord a) => ThreeTuple a -> ThreeTuple a -> ThreeTuple a
tupleSum (ThreeTuple a1 a2 a3 d1) (ThreeTuple b1 b2 b3 d2) = ThreeTuple (a1+b1) (a2+b2) (a3+b3) d1

tupleMultiply :: (Floating a, Ord a) => ThreeTuple a -> ThreeTuple a -> ThreeTuple a
tupleMultiply (ThreeTuple a1 a2 a3 d1) (ThreeTuple b1 b2 b3 d2) = ThreeTuple (a1*b1) (a2*b2) (a3*b3) d1
--2
coordinateDoubler :: (Floating a, Ord a) => ThreeTuple a -> ThreeTuple a
coordinateDoubler = tupleMultiply (ThreeTuple 2 2 2 0)

coordinateNegate :: (Floating a, Ord a) => ThreeTuple a -> ThreeTuple a 
coordinateNegate = tupleMultiply (ThreeTuple (-1) (-1) (-1) 0)

--3
zeroNeg :: (Floating t, Ord t) => ThreeTuple t -> ThreeTuple t
zeroNeg (ThreeTuple x y z d)
    | x < 0 || y < 0 || z < 0 = (ThreeTuple 0 0 0 d)
    | otherwise = (ThreeTuple x y z d)

zeroNegList ps = map zeroNeg ps

--4
zeroNegList2 ps = map (\(ThreeTuple x y z d) -> if x<0 || y<0 || z<0 then (ThreeTuple 0 0 0 d) else (ThreeTuple x y z d)) ps

--5
tupeListSum ps = foldl (\acc p -> tupleSum acc p) (ThreeTuple 0 0 0 0) ps
