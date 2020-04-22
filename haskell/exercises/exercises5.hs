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
-- = "(67+(-37))"


-- Exercise 14.16 Page 255
data Expr = Lit Int |
            Add Expr Expr |
            Sub Expr Expr |
            Mul Expr Expr |
            Div Expr Expr 

size :: Expr -> Int
size (Lit _) = 0
size (Add le re) = 1 + size le + size re
size (Sub le re) = 1 + size le + size re


-- Exercise 14.17 Page 255
size (Mul le re) = 1 + size le + size re
size (Div le re) = 1 + size le + size re

eval :: Expr -> Int
eval (Lit n) = n
eval (Add le re) = eval le + eval re
eval (Sub le re) = eval le - eval re
eval (Mul le re) = eval le * eval re
eval (Div le re) = div (eval le) (eval re)

showE :: Expr -> String
showE (Lit n) = show n
showE (Add le re) = "(" ++ showE le ++ "+" ++ showE re ++ ")"
showE (Sub le re) = "(" ++ showE le ++ "-" ++ showE re ++ ")"
showE (Mul le re) = "(" ++ showE le ++ "*" ++ showE re ++ ")"
showE (Div le re) = "(" ++ showE le ++ "/" ++ showE re ++ ")"


-- Exercise 14.18 Page 255
data Expr2 = Lit2 Int |
             Op Ops Expr Expr
data Ops = Add2 | Sub2 | Mul2 | Div2

eval2 :: Expr2 -> Int
eval2 (Lit2 n) = n
eval2 (Op op le re) =
 case op of Add2 -> lv + rv
            Sub2 -> lv - rv
            Mul2 -> lv * rv
            Div2 -> div lv rv
  where
   lv = eval le
   rv = eval re


-- Exercise 14.21 Page 256
data NTree = NilT |
             Node Int NTree NTree

getSubTrees :: NTree -> (NTree, NTree)
getSubTrees (NilT) = error "No sub trees in leaf"
getSubTrees (Node _ lt rt) = (lt, rt)


-- Exercise 14.22 Page 256
elemTree :: Int -> NTree -> Bool
elemTree _ (NilT) = False
elemTree n (Node v lt rt)
 | n == v = True
 | elemTree n lt = True 
 | elemTree n rt = True
 | otherwise = False


-- Exercise 14.23 Page 256
isNilT :: NTree -> Bool
isNilT (NilT) = True
isNilT _ = False

setMinMax :: Int -> (Int, Int) -> (Int, Int)
setMinMax n (x, y) = (mi, ma)
 where
  mi = if n < x then n else x
  ma = if n > y then n else y

setMiMa :: (Int, Int) -> (Int, Int) -> (Int, Int)
setMiMa (x1, y1) (x2, y2) = (mi, ma)
 where
  mi = if x1 < x2 then x1 else x2
  ma = if y1 > y2 then y1 else y2

minMax :: NTree -> (Int, Int)
minMax (NilT) = error "No values in leaf"
minMax (Node v le re)
 | isLt && isRt = (v, v)
 | isLt = setMinMax v (minMax le)
 | isRt = setMinMax v (minMax re)
 | otherwise = setMiMa (minMax le) (setMinMax v (minMax re))
  where
   isLt = isNilT le
   isRt = isNilT re


-- Exercise 14.24 Page 256
reflect :: NTree -> NTree
reflect (NilT) = NilT
reflect (Node v le re) = Node v (reflect re) (reflect le)


-- Exercise 14.28 Page 260
data Tree a = Nil | NodeT a (Tree a) (Tree a)
              deriving (Eq, Ord, Show, Read)

isLeaf :: Tree a -> Bool
isLeaf (Nil) = True
isLeaf _ = False

elemT :: Eq a => a -> Tree a -> Bool
elemT _ (Nil) = False
elemT n (NodeT v lt rt)
 | n == v = True
 | otherwise = elemT n lt || elemT n rt


-- Exercise 14.29 Page 260
twist :: Either a b -> Either b a
twist (Left l) = Right l
twist (Right r) = Left r 


-- Exercise 14.33 Page 260
data GTree a = Leaf a | Gnode [GTree a] deriving Show

countGT :: GTree a -> Int
countGT (Leaf _) = 1
countGT (Gnode xs) = foldr (\ x y -> y + countGT x) 0 xs

depthGT :: GTree a -> Int
depthGT (Leaf _) = 0
depthGT (Gnode xs) = 1 + foldr (\ x y -> (max (countGT x) y)) 0 xs

sumGT :: Num a => GTree a -> a
sumGT (Leaf v) = v
sumGT (Gnode xs) = foldr (\ x y -> y + sumGT x) 0 xs

elemGT :: Eq a => a -> GTree a -> Bool
elemGT n (Leaf v) = n == v
elemGT n (Gnode xs) = foldr (\ x y -> y || elemGT n x) False xs

mapGT :: (a -> b) -> GTree a -> GTree b
mapGT f (Leaf v) = Leaf (f v)
mapGT f (Gnode xs) = Gnode (map (mapGT f) xs)

flattenGT :: GTree a -> [a]
flattenGT (Leaf v) = [v]
flattenGT (Gnode xs) = foldr (++) [] (map flattenGT xs)


-- Exercise 14.34 Page 260
-- Empty tree is represented by:  Gnode []

