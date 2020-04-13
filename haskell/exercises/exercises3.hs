-- Exercise 9.2 Page 159
myLength :: [Int] -> Int
myLength xs = sum (map one xs)
 where
  one :: Int -> Int
  one _ = 1
  

-- Exercise 9.4 Page 160
-- Let xs :: [a]
-- Let g :: a -> b
-- Let f :: b -> c
-- Then map f (map g xs) gives :: [c]
-- So we preform g then g on every value of xs to get the result of type [c]. 
-- Try to wtite map f (map g xs) using just one map.
fg :: (b -> c) -> (a -> b) -> [a] -> [c]
fg f g xs = map (f . g) xs


-- Exercise 9.6 Page 160
squareList1 :: [Int] -> [Int]
squareList1 ns = map (**) ns
 where
  (**) :: Int -> Int
  (**) x = x * x

squareList2 :: [Int] -> [Int]
squareList2 ns = [ x * x | x <- ns]

sumSquareList1 :: [Int] -> Int
sumSquareList1 ns = foldr (+) 0 (squareList1 ns)

greaterThenZero1 :: [Int] -> Bool
greaterThenZero1 ns = foldr (&&) True nns
 where
  nns = map (\ x -> x > 0) ns

greaterThenZero2 :: [Int] -> Bool
greaterThenZero2 ns = foldr (&&) True nns
 where
  nns = [ x > 0 | x <- ns]


-- Exercise 9.7 Page 160



-- Exercise 9.9 Page 160-161
-- Exercise 9.10 Page 161
-- Exercise 9.11 Page 163
-- Exercise 9.16 Page 164
-- Exercise 9.17 Page 164
-- Exercise 10.3 Page 171
-- Exercise 10.7 Page 175
-- Exercise 10.8 Page 175
-- Exercise 10.13 Page 180
-- Exercise 10.14 Page 183

-- Exercise 10.20-10.32 Page 191-193
-- Exercise 10.22 Page 191=192

