-- Exercise 14.1 Page 248
data Temp = Cold | Hot deriving Show
data Season = Spring | Summer | Autumn | Winter deriving Eq

weather :: Season -> Temp
weather w = case w of Summer -> Hot
                      _ -> Cold

weather1 :: Season -> Temp
weather1 w = if w == Summer then Hot else Cold


-- Exercise 14.4 Page 248
data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float 
             deriving (Eq, Ord, Show, Read)

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle l w) = 2 * (l + w)


-- Exercise 14.5 Page 248
perimeter (Triangle a b c) = a + b + c

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False
isRound (Triangle _ _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle l w) = l * w
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
 where
  s = (a + b + c)/2.0


-- Exercise 14.6 Page 248
isRegular :: Shape -> Bool
isRegular (Circle _) = True
isRegular (Rectangle l w) = l == w
isRegular (Triangle a b c) = a == b && b == c


-- Exercise 14.8 Page 248
class MyEq a where
 myEq :: a -> a -> Bool

instance MyEq Shape where
 myEq (Circle r1) (Circle r2) = r1 == r2 || (r1 < 0 && r2 < 0)
 myEq (Rectangle l1 w1) (Rectangle l2 w2)
  | l1 == l2 && w1 == w2 = True
  | (l1 < 0 || w1 < 0) && (l2 < 0 || w2 < 0) = True
  | otherwise = False
 myEq (Triangle a1 b1 c1) (Triangle a2 b2 c2)
  | a1 == a2 && b1 == b2 && c1 == c2 = True
  | (a1 < 0 || b1 < 0 || c1 < 0) && (a2 < 0 || b2 < 0 || c2 < 0) = True
  | otherwise = False

-- Exercise 14.9 Page 248
type Point = (Float, Float)

data NewShape = PShape Point Shape deriving (Eq, Ord, Show, Read)


-- Exercise 14.10 Page 249
move :: Float -> Float -> NewShape -> NewShape
move dx dy (PShape (x,y) s) = PShape (x + dx, y + dy) s


-- Exercise 14.15 Page 255
-- 1. eval (Lit 67)
-- eval :: Expr -> Int
-- Lit :: Int -> Expr
-- 67 :: Int
-- Lit 67 :: Expr
-- eval (Lit 67) :: Int = 67
--
-- 2. eval (Add (Sub (Lit 3) (Lit 1)) (Lit 3))
-- Add :: Expr -> Expr -> Expr
-- Sub :: Expr -> Expr -> Expr
-- eval (Add (Sub (Lit 3) (Lit 1)) (Lit 3)) :: Expr -> Int 
-- = (3-1)+3 = 5
--
-- 3. show (Add (Lit 67) (Lit (-37)))
-- show :: Show a => a -> String
-- show (Add (Lit 67) (Lit (-37))) :: Expr -> String
-- = "67+(-37)"


-- Exercise 14.16 Page 255
data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr

size :: Expr -> Int
size (Lit _) = 1
size (Add le re) = 1 + size le + size re
size (Sub le re) = 1 + size le + size re


-- Exercise 14.17 Page 255
-- Exercise 14.18 Page 255
-- Exercise 14.21 Page 256
-- Exercise 14.22 Page 256
-- Exercise 14.23 Page 256
-- Exercise 14.24 Page 256
-- Exercise 14.28 Page 260
-- Exercise 14.29 Page 
-- Exercise 14.33 Page 260
-- Exercise 14.34 Page 260

