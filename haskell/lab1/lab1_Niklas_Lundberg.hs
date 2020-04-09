rmLast :: [Int] -> [Int]
rmLast (x:xs)
 | length (x:xs) < 2 = []
 | otherwise = x : rmLast xs

sumArr :: [int] -> int
sumArr [] = 0
sumArr (x:xs) = x + sumArr xs

allSubArr :: [Int] -> [[Int]]
allSubArr [] = []
allSubArr (x:xs) = hallSubArr (x:xs) ++ allSubArr xs
 where 
  hallSubArr :: [Int] -> [[Int]]
  hallSubArr [] = []
  hallSubArr xs = xs : hallSubArr (rmLast xs)

