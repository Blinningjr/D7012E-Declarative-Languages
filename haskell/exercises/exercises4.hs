-- Exercise 12.2 Page 214
numEqual :: Eq a => [a] -> a -> Int
numEqual xs x = foldr (+) 0 (map (\ n -> if n == x then 1 else 0) xs)

member :: Eq a => [a] -> a -> Bool
member xs x = numEqual xs x > 0


-- Exercise 12.3 Page 214
oneLookupFirst :: Eq a => [(a, b)] -> a -> b
oneLookupFirst [] _ = error "No such pair in the list."
oneLookupFirst ((f,s):rest) x
 | f == x = s
 | otherwise = oneLookupFirst rest x

oneLookupSecond :: Eq b => [(a, b)] -> b -> a
oneLookupSecond [] _ = error "No such pair in the list"
oneLookupSecond ((f,s):rest) x
 | s == x = f
 | otherwise = oneLookupSecond rest x  


-- Exercise 12.4 Page 219
class Visible a where
 toString :: a -> String
 size :: a -> Int

instance Visible Bool where
 toString True = "True"
 toString False = "False"
 size _ = 1

instance (Visible a, Visible b) => Visible (a, b) where
 toString (f, s) = "(" ++ toString f ++ ", " ++ toString s ++ ")"
 size (f, s) = size f + size s

instance (Visible a, Visible b, Visible c) => Visible  (a, b, c) where
 toString (f, s, t) = "(" ++ toString f ++ ", " ++ toString s ++ ", " ++ toString t ++ ")"
 size (f, s, t) = size f + size s + size t


-- Exercise 12.5 Page 219
toStringInt :: Int -> String
toStringInt 0 = "0"
toStringInt 1 = "1"
toStringInt 2 = "2"
toStringInt 3 = "3"
toStringInt 4 = "4"
toStringInt 5 = "5"
toStringInt 6 = "6"
toStringInt 7 = "7"
toStringInt 8 = "8"
toStringInt 9 = "9"
toStringInt x = toStringInt (div x 10) ++ toStringInt (mod x 10)

instance Visible Integer where
 toString x = toStringInt (fromInteger x)
 size x = 2 * (fromInteger x)


-- Exercise 12.6 Page 219
-- What is the type of the function
-- compare x y = size x <= size y 
--
-- size :: Visible a => a -> Int
-- (<=) :: Ord a => a -> a -> Bool  In this case  Int -> Int -> Bool
-- compare x y :: (Visible a, Visible b) => a -> b -> Bool 


-- Exercise 12.8 Page 219
--instance (Ord a, Ord b) => Ord (a, b) where
-- (<) (x, y) = y > x
-- (<=) (x, y) = y > x || x == y
-- (>) (x, y) = y < x
-- (>=) (x, y) = y < x || x == y
-- max (x, y) = if x > y then x else y
-- min (x, y) = if x > y then y else x
-- compare (x, y)
--  | x < y = LT
--  | x == y = EQ
--  | x > y = GT
--
--instance Ord b => Ord [b] where
 

-- Exercise 12.10 Page 225
-- Exercise 13.1 Page 230
-- Exercise 13.2 Page 237
-- Exercise 13.3 Page 237
-- Exercise 13.4 Page 237
-- Exercise 13.5 Page 237
-- Exercise 13.6 Page 237-238
-- Exercise 13.7 Page 238
-- Exercise 13.8 Page 238
-- Exercise 13.10 Page 240
-- Exercise 13.12 Page 240

