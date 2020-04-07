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

