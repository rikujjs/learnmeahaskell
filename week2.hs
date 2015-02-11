-- Ex 1
tupleSum :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
tupleSum (a1,a2,a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)

tupleMultiply :: (Num a) => (a,a,a) -> (a,a,a) -> (a,a,a)
tupleMultiply (a1,a2,a3) (b1, b2, b3) = (a1*b1, a2*b2, a3*b3)

-- Ex 2
coordinateDoubler :: (Num a) => (a,a,a) -> (a,a,a)
coordinateDoubler = tupleMultiply (2,2,2)

coordinateNegate :: (Num a) => (a,a,a) -> (a,a,a)
coordinateNegate = tupleMultiply (-1,-1,-1)

-- Ex 3
doubleOrZeroesMap :: (Num a, Ord a) => [(a,a,a)] -> [(a,a,a)]
doubleOrZeroesMap [] = []
doubleOrZeroesMap xs = map doubleOrZeroes xs

doubleOrZeroes :: (Num a, Ord a) => (a,a,a) -> (a,a,a)
doubleOrZeroes (x,y,z)
    | x < 0 || y < 0 || z < 0 = (0,0,0)
    | otherwise = coordinateDoubler (x,y,z)

-- Ex 4
doubleOrZeroesLambda :: (Num a, Ord a) => [(a,a,a)] -> [(a,a,a)]
doubleOrZeroesLambda [] = []
doubleOrZeroesLambda xs =
    map (\(a,b,c) -> if a < 0 || b < 0 || c < 0 then (0,0,0) else (2*a,2*b,2*c)) xs

-- Ex 5
tupleListSum :: (Num a) => [(a,a,a)] -> (a,a,a)
tupleListSum [] = (0,0,0)
tupleListSum xs =
    foldl (\(a1,b1,c1) (a2,b2,c2) -> (a1 + a2, b1 + b2, c1 + c2)) (0,0,0) xs
