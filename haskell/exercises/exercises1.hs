--import Prelude

-- Exercies 3.7 Page 37
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent m n p = m /= n && m /= p && n /= p
-- threeDifferent 3 4 3 = False, m and p have the same value.

-- Implement threeDifferent1 with guards.
threeDifferent1 :: Int -> Int -> Int -> Bool
threeDifferent1 m n p
 | m == n = False
 | m == p = False
 | n == p = False
 | otherwise = True


-- Implement threeDiffrent2 with conditional expressions.
threeDifferent2 :: Int -> Int -> Int -> Bool
threeDifferent2 m n p = 
 if m /= n then if m /= p then if n /= p then True else False else False else False


-- Exercies 3.8 Page 37
twoEqual :: Int -> Int -> Bool
twoEqual m n = m == n

-- Use twoEqual to define threeEqual.
threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = twoEqual m n && twoEqual n p

-- Use a combination of twoEqual and threeEqual to define fourEqual.
fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual m n p q = threeEqual m n p && twoEqual p q


-- Exercies 3.17 Page 46
square :: Float -> Float
square n = n * n

smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
 | pow < 0 = 0 
 | otherwise = ((-b) - (sqrt pow))/(2 * a)
  where pow = ((square b) - 4 * a * c) 

largerRoot a b c
 | pow < 0 = 0 
 | otherwise = ((-b) + (sqrt pow))/(2 * a)
  where pow = ((square b) - 4 * a * c) 


-- Implement as one function that returns real and imagenary parts.
myAbs :: Float -> Float
myAbs n 
 | n >= 0 = n
 | otherwise = -n

quadraticRoot :: Float -> Float -> Float -> ((Float, Float), (Float, Float))
quadraticRoot a b c
 | pow >= 0 = ((real - imag, 0), (real + imag, 0))
 | otherwise = ((real, (-imag)), (real, imag))
  where 
   real = (-b)/(2.0 * a)
   imag = (sqrt (myAbs pow))/(2.0 * a)
   pow = pow
    where pow = ((square b) - 4.0 * a * c) 


-- Exercies 4.7 Page 64
mulNN :: Int -> Int -> Int
mulNN 0 _ = 0
mulNN a b = b + (a-1) `mulNN` b
-- a should be defined for all natural numbers
-- Base case is when a is 0 because it is the smalles natural number.


-- Exercies 4.8 Page 64
myIntSqrt :: Int -> Int -> Int
myIntSqrt k n
 | (k * k) > n = k - 1
 | otherwise = myIntSqrt (k+1) n


-- Exercies 4.9 Page 64
f :: Int -> Int
f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

maxF :: Int -> Int
maxF 0 = f 0
maxF n
 | a > b = a
 | otherwise = b  
  where 
   a = f n
   b = maxF (n-1)

-- Do a recursive maxF with a list as input
arrF :: Int -> [Int]
arrF 0 = [f 0]
arrF n = (arrF (n-1)) ++ [f n]

maxFl :: [Int] -> Int
maxFl [first] = f first 
maxFl (x:xs)
 | x < tail = tail
 | otherwise = x
  where tail = maxFl xs


-- Exercise 4.14 Page 67
squareI :: Int -> Int
squareI n = n * n

myAbsI :: Int -> Int
myAbsI n
 | n < 0 = (-n)
 | otherwise = n

myMod :: Int -> Int -> Int
myMod n k = hmod n k 1
 where 
  hmod :: Int -> Int -> Int -> Int
  hmod n k x
   | k * x > n = n - k * (x-1)
   | otherwise = hmod n k (x+1)

powerOfTwo :: Int -> Int
powerOfTwo 0 = 1
powerOfTwo n
 | n `myMod` 2 == 0 = squareI (powerOfTwo (n `div` 2))
 | otherwise = (squareI (powerOfTwo ((n-1) `div` 2))) * 2

