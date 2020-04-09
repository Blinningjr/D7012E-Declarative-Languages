-- Removes the last element in a list
rmLast :: [Int] -> [Int]
rmLast (x:xs)
 | length (x:xs) < 2 = []
 | otherwise = x : rmLast xs

-- Sums all elements in a list
sumArr :: [Int] -> Int
sumArr [] = 0
sumArr (x:xs) = x + sumArr xs

-- Takes the first k elements from a list
takeFirstK :: Int -> [a] -> [a]
takeFirstK _ [] = []
takeFirstK k (x:xs)
 | k > 0 = x : takeFirstK (k-1) xs
 | otherwise = []

-- Sorts a list of all sublists on the size 
quicksort :: [(Int, (Int, Int), [Int])] -> [(Int, (Int, Int), [Int])]
quicksort [] = []
quicksort (x:xs) = quicksort lhs ++ [x] ++ quicksort rhs
 where
  pivot :: (Int, (Int, Int), [Int]) -> [(Int, (Int, Int), [Int])] -> ([(Int, (Int, Int), [Int])], [(Int, (Int, Int), [Int])])
  pivot _ [] = ([], []) 
  pivot p (x:xs)
   | xSize < pSize = (x : lhs, rhs) 
   | otherwise = (lhs, x : rhs)
    where
     (xSize, _, _) = x
     (pSize, _, _) = p
     (lhs, rhs) = pivot p xs 
  
  (lhs, rhs) = pivot x xs

  

-- Calculates all possible sublists in form [(Size, (i, j), [SubList])]
allSubArr :: [Int] -> [(Int, (Int, Int), [Int])]
allSubArr [] = []
allSubArr xs = hallSubI (1, length xs) xs
 where 
  hallSubJ :: (Int, Int) -> [Int] -> [(Int, (Int, Int), [Int])]
  hallSubJ _ [] = []
  hallSubJ (i, j) xs = (sumArr xs, (i, j), xs) : hallSubJ (i, j-1) (rmLast xs)
  
  hallSubI :: (Int, Int) -> [Int] -> [(Int, (Int, Int), [Int])]
  hallSubI _ [] = []
  hallSubI (i, j) (x:xs) = hallSubJ (i, j) (x:xs) ++ hallSubI (i+1, j) xs

