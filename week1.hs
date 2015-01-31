-- Ex 1, calculate sum of positive numbers in a list
sumPositives numList = sum [ c | c <- numList, c > 0 ]


-- Ex 2
followedByGreater ::  [Int] -> [Int]
followedByGreater list = [a | (a,b) <- (zip list (tail list)), a < b] 

-- Ex 3
--Helpers
first :: (a,b,c) -> a
first (a,_,_) = a

second :: (a,b,c) -> b
second (_,b,_) = b

third :: (a,b,c) -> c
third (_,_,c) = c

euclidianDistance :: (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int
euclidianDistance x y distance = sqrt (((first x - first y)^2) + ((second x - second y)^2) + ((third x - third y)^2))


