-- Ex 1, calculate sum of positive numbers in a list
sumPositives numList = sum [ c | c <- numList, c > 0 ]

-- Ex 2
followedByGreater ::  [Int] -> [Int]
followedByGreater list = [a | (a,b) <- (zip list (tail list)), a < b] 

-- Ex 3
euclidianDistance :: (Ord a, Floating a) => (a,a,a) -> (a,a,a) -> a -> Bool
euclidianDistance (x1, y1, z1) (x2, y2, z2) distance = sqrt (((x1 - x2)^2) + ((y1 - y2)^2) + ((z1 - z2)^2)) <= distance

-- Ex 4
--helper
--euclidianDistance :: (Ord a, Floating a) => (a,a,a) -> (a,a,a) -> a -> Bool
--euclidianDistance (x1, y1, z1) (x2, y2, z2) distance = sqrt (((x1 - x2)^2) + ((y1 - y2)^2) + ((z1 - z2)^2)) <= distance


distanceCalculator :: (Ord a, Floating a) => (a, a, a) -> [(a, a, a)] -> a -> [(a,a,a)]
--use the help of previous excercise to figure out if within range
distanceCalculator (x1, y1, z1) positionList d = [(a, b, c) | (a,b,c) <- positionList, euclidianDistance (a, b, c) (x1, y1, z1) d]

-- Ex 5
--clusterChecker :: (Ord a, Floating a) => [(a,a,a)] -> a -> Bool
--clusterChecker positionList d = 


clusterChecker :: (Ord a, Floating a) => [(a,a,a)] -> a -> Bool
clusterChecker positionList d
     | length [1 | x <- positionList, x `elem` allDist] == length positionList = True
     | otherwise = False
     where allDist = [(x1,y1,z1) | (x1,y1,z1) <- positionList, (x2,y2,z2) <- positionList, (x1,y1,z1) /= (x2,y2,z2), euclidianDistance (x1,y1,z1) (x2,y2,z2) d]
