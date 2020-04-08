-- Exercise 5.2 Page 77
orderPair :: (Int, Int) -> (Int, Int)
orderPair (a, b)
 | a > b = (b, a)
 | otherwise = (a, b)

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) = (h, i, g)
  where 
   (d, e) = orderPair (a, b)
   (f, g) = orderPair (e, c)
   (h, i) = orderPair (d, f) 


-- Exercise 5.10 Page 82
myMod :: Int -> Int -> Int
myMod n k = hmod n k 1
 where 
  hmod :: Int -> Int -> Int -> Int
  hmod n k x
   | k * x > n = n - k * (x-1)
   | otherwise = hmod n k (x+1)

divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n], (myMod n x) == 0 ] 

isPrime :: Int -> Bool
isPrime n = if divisors n == [1, n] then True else False


-- Exercise 5.11 Page 82-83
matches :: Int -> [Int] -> [Int]
matches n l = [ x | x <- l, x == n ]

myElem :: Int -> [Int] -> Bool
myElem n l
 | length (matches n l) > 0 = True
 | otherwise = False


-- Exercise 5.18 Page 90
shift ((x, y), z) = (x, (y, z))
-- Guess: shift :: ((a, b), c) -> (a, (b, c))
-- Type: shift :: ((a1, a2), b) -> (a1, (a2, b))


--Exercise 5.22 Page 94
onSeparateLines :: [String] -> String
onSeparateLines [] = ""
onSeparateLines (x:xs) = x ++ "\n" ++ onSeparateLines xs


-- Exercise 5.23 Page 94
duplicate :: String -> Int -> String
duplicate s n
 | n < 1 = ""
 | otherwise = s ++ duplicate s (n-1)


-- Exercise 5.24 Page 94
pushRight :: Int -> String -> String
pushRight l s
 | length s < l = pushRight l (" " ++ s)
 | otherwise = s


-- Exercise 6.29 Page 112-113
count :: Int -> [Int] -> Int
count n [] = 0
count n [x]
 | n == x = 1
 | otherwise = 0
count n (x:xs)
 | n == x = 1 + count n xs
 | otherwise = count n xs

containsN :: Int -> [Int] -> Bool
containsN n l
 | length [ x | x <- l, n == x] == 0 = False
 | otherwise = True

removeDuplicate :: [Int] -> [Int]
removeDuplicate [] = []
removeDuplicate [x] = [x]
removeDuplicate (x:xs) = hrmDup xs [x] 
 where
  hrmDup :: [Int] -> [Int] -> [Int]
  hrmDup [] l = l
  hrmDup (x:xs) l = if containsN x l then hrmDup xs l else hrmDup xs (l ++ [x])

countProduct :: [Int] -> [(Int, Int)]
countProduct l = [ (x, count x l) | x <- unique ]
 where 
  unique = removeDuplicate l
-- Not done but will stop here.


-- Exercise 7.2 Page 119
addFirstTwo :: [Int] -> Int
addFirstTwo [] = 0
addFirstTwo [first] = first
addFirstTwo (a:b:_) = a + b


-- Exercise 7.3 Page 119
addFirstTwo1 :: [Int] -> Int
addFirstTwo1 xs
 | length xs == 0 = 0
 | length xs == 1 = xs !! 0
 | otherwise = (xs !! 0) + (xs !! 1)

