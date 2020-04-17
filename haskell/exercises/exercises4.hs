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



-- Exercise 12.5 Page 219
-- Exercise 12.6 Page 219
-- Exercise 12.8 Page 219
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

