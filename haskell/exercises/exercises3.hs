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
minFOfN :: (Int -> Int) -> Int -> Int
minFOfN f n
 | n < 1 = f n
 | f n <= prev = f n
 | otherwise = prev
  where
   prev = minFOfN f (n-1)

allEq :: [Int] -> Bool
allEq [] = True
allEq (x:xs) = hallEq xs x
 where
  hallEq :: [Int] -> Int -> Bool
  hallEq [] _ = True
  hallEq (x:xs) n = x == n && hallEq xs n
  
fEqual :: (Int -> Int) -> Int -> Bool
fEqual f n = allEq (map f [0..n]) 

fGreater :: (Int -> Int) -> Int -> Bool
fGreater f n = res 
 where
  greater :: [Int] -> (Int, Bool)
  greater [] = (0, True)
  greater (x:xs) = (x, x > p && b)
   where
    (p, b) = greater xs

  (_, res) = greater [ f i | i <- reverse [0..n]]  


-- Exercise 9.9 Page 160-161
iter :: Int -> (a -> a) -> a -> a
iter 0 _ x = x
iter n f x = f (iter (n-1) f x)

iter2 :: Int -> (a -> a) -> (a -> a)
iter2 0 _ = (\ x -> x)
iter2 n f = f . (iter2 (n-1) f)


-- Exercise 9.10 Page 161
double :: Int -> Int
double x = 2 * x

pow2 :: Int -> Int
pow2 n = iter n double 1


-- Exercise 9.11 Page 163
sumSquare :: Int -> Int
sumSquare n = foldr (+) 0 (map (\ x -> x*x) [1..n])


-- Exercise 9.16 Page 164
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x:xs)
 | p x = x : filterFirst p xs
 | otherwise = xs

returnLoan :: (a -> Bool) -> [a] -> a
returnLoan _ [] = error "Could not find a element in the list that have the propery"
returnLoan p (x:xs)
 | length (x:xs) == length filterd = x
 | otherwise = returnLoan p filterd 
  where
   filterd = filterFirst p (x:xs)


-- Exercise 9.17 Page 164
filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p xs = reverse (filterFirst p (reverse xs))


-- Exercise 10.3 Page 171
-- Exercise 10.7 Page 175
-- Exercise 10.8 Page 175
-- Exercise 10.13 Page 180
-- Exercise 10.14 Page 183

-- Exercise 10.20-10.32 Page 191-193
-- Exercise 10.22 Page 191=192

