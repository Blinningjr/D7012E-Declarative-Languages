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
composeList :: [(a -> a)] -> (a -> a)
composeList [] = (\ x -> x)
composeList (x:xs) = x . composeList xs

composeList1 :: [(a -> a)] -> (a -> a)
composeList1 xs = foldr (.) (\ x -> x) xs


-- Exercise 10.7 Page 175
flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f = (\ a b -> f b a)


-- Exercise 10.8 Page 175
nonWhitespace :: Char -> Bool
nonWhitespace = not . (\ c -> elem c " \t\n")


-- Exercise 10.13 Page 180
mf = (map (+1) . filter (>=0))


-- Exercise 10.14 Page 183
whiteS :: String
whiteS = "  "

blackS :: String
blackS = "##"

-- Not Needed
wbS :: String
wbS = whiteS ++ blackS

-- Not Needed
bwS :: String
bwS = blackS ++ whiteS

nSW :: Int -> String
nSW n
 | n < 1 = "\n"
 | otherwise = whiteS ++ nSB (n-1)

nSB :: Int -> String
nSB n
 | n < 1 = "\n"
 | otherwise = blackS ++ nSW (n-1)

chessBoard :: Int -> String
chessBoard n = foldr (++) "" [ if even x then nSB n else nSW n | x <- [1..n]]


-- Exercise 10.20 Page 191
type Doc = String
type Line = String
type MyWord = String

myLines :: Doc -> [Line]
myLines [] = []
myLines st = [takeWhile f st] ++ myLines (filterFirst f (dropWhile f st))
 where
  f = (/='\n')


-- Exercise 10.21 Page 191
makeLists1 :: [(Int, MyWord)] -> [([Int], MyWord)]
makeLists1 = map (\ (n, st) -> ([n], st))

makeLists2 :: [(Int, MyWord)] -> [([Int], MyWord)]
makeLists2 l = [ ([n], st) | (n, st) <- l]

shorten1 :: [(Int, MyWord)] -> [(Int, MyWord)]
shorten1 = filter (\ (nl, wd) -> length wd > 3)

shorten2 :: [(Int, MyWord)] -> [(Int, MyWord)]
shorten2 l = [ (nl, wd) | (nl, wd) <- l, length wd > 3]


-- Exercise 10.22 Page 191-192
toTup :: [Int] -> [(Int, Int)]
toTup xs = [ (x, x) | x <- xs]

getEnd :: [(Int, Int)] -> Int -> Int
getEnd [] n = n
getEnd (x:xs) n
 | n + 1 == s = getEnd xs s
 | otherwise = n
  where
   (s, _) = x

rmTo :: [(Int, Int)] -> Int -> [(Int, Int)]
rmTo (x:xs) n
 | s < n = rmTo xs n
 | otherwise = xs
  where
   (s, e) = x

hcompact :: [(Int, Int)] -> [(Int, Int)]
hcompact [] = []
hcompact (x:xs)
 | s == e = x : hcompact xs
 | otherwise = (s, e) : hcompact (rmTo xs e)
  where
   (s, _) = x
   e = getEnd xs s

compact :: [Int] -> String
compact l = foldr (++) "" (x : map (\ st -> ", " ++ st) xs)
 where 
  (x:xs) = map (\ (s, e) -> if s == e then show s else (show s) ++ "-" ++ (show e)) (hcompact (toTup l))


-- Exercise 10.23 Page 192
orderPair :: (Int, MyWord) -> (Int, MyWord) -> Bool
orderPair (n1, w1) (n2, w2) = w1 < w2 || (w1 == w2 && n1 < n2)

sortLs :: [(Int, MyWord)] -> [(Int, MyWord)]
sortLs [] = []
sortLs (p:ps) = sortLs smaller ++ [p] ++ sortLs larger
 where
  smaller = [ q | q <- ps, orderPair q p || p == q]
  larger = [ q | q <- ps, orderPair p q]


-- Exercise 10.24 Page 192
amalgamate :: [([Int], MyWord)] -> [([Int], MyWord)]
amalgamate [] = []
amalgamate [p] = [p]
amalgamate ((l1, w1):rest) = (reverse ll, rl)  : (amalgamate left)
 where
  taken = reverse (takeWhile (\ (_, w) -> w == w1) rest)
  left = dropWhile (\ (_, w) -> w == w1) rest
  (ll, rl) = (foldr (\ (ll, lw) (rl, rw) -> (ll ++ rl, lw)) (l1, w1) taken) 


-- Exercise 10.25 Page 192
mySizer :: (a, String) -> Bool
mySizer = (>3) . length . snd


-- Exercise 10.26 Page 192
amalgamateF :: [([Int], MyWord)] -> [([Int], MyWord)]
amalgamateF [] = []
amalgamateF [p] = [p]
amalgamateF ((l1,w1):(l2,w2):rest)
 | w1 /= w2 = (l1, w1) : amalgamateF ((l2,w2):rest)
 | otherwise = (l1++l2, w1) : amalgamateF rest -- This line is faulty
-- This will result in (a++b, w1), (c, w1), when it shuld be (a++b++c, w1)


-- Exercise 10.27 Page 192
padRight :: String -> Int -> String
padRight st n
 | n - length st < 1 = st
 | otherwise = padRight (st ++ " ") n 

showIndex :: ([Int], MyWord) -> Int -> String
showIndex (l, w) n = (padRight w n) ++ (compact l) ++ "\n"

findLongest :: [([Int], MyWord)] -> Int
findLongest [] = 0
findLongest ((_, w):xs)
 | wLen < prevLong = prevLong
 | otherwise = wLen
  where
   wLen = length w
   prevLong = findLongest xs

showIndexs :: [([Int], MyWord)] -> String
showIndexs l = "\n" ++ foldr (++) "\n" [ showIndex x maxLen | x <- l]
 where
  maxLen = (findLongest l) + 2

printIndex :: [([Int], MyWord)] -> IO ()
printIndex i = putStr (showIndexs i)


-- Exercise 10.28 Page 192



-- Exercise 10.29 Page 192
-- Exercise 10.30 Page 193
-- Exercise 10.31 Page 193
-- Exercise 10.32 Page 193

