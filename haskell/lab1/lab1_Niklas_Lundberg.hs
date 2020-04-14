-- Niklas Lundberg

---------------------------------------------------------------------------------------------------
-- Functions for calculating all the sublists of a list -------------------------------------------
---------------------------------------------------------------------------------------------------

-- Removes the last element in a list
rmLast :: [Int] -> [Int]
rmLast (x:xs)
 | length (x:xs) < 2 = []
 | otherwise = x : rmLast xs

-- Sums all elements in a list
sumArr :: [Int] -> Int
sumArr [x]= x
sumArr (x:xs) = x + sumArr xs

-- Calculates all possible sublists in form [(Size, (i, j), [SubList])]
allSubArr :: [Int] -> [(Int, (Int, Int), [Int])]
allSubArr xs = hallSubI (1, length xs) xs
 where 
  hallSubJ :: (Int, Int) -> [Int] -> [(Int, (Int, Int), [Int])]
  hallSubJ _ [] = []
  hallSubJ (i, j) xs = (sumArr xs, (i, j), xs) : hallSubJ (i, j-1) (rmLast xs)
  
  hallSubI :: (Int, Int) -> [Int] -> [(Int, (Int, Int), [Int])]
  hallSubI (i, j) [x] = [(x, (i, j), [x])]
  hallSubI (i, j) (x:xs) = hallSubJ (i, j) (x:xs) ++ hallSubI (i+1, j) xs

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Functions for getting the k smallest set of all the sublists -----------------------------------
---------------------------------------------------------------------------------------------------

-- Sorts a list of all sublists on the size 
quicksort :: [(Int, (Int, Int), [Int])] -> [(Int, (Int, Int), [Int])]
quicksort [] = []
quicksort (x:xs) = quicksort lhs ++ [x] ++ quicksort rhs
 where
  pivot :: (Int, (Int, Int), [Int]) -> [(Int, (Int, Int), [Int])] -> ([(Int, (Int, Int), [Int])], [(Int, (Int, Int), [Int])])
  pivot _ [] = ([], []) 
  pivot p (x:xs)
   | xSize <= pSize = (x : lhs, rhs) 
   | otherwise = (lhs, x : rhs)
    where
     (xSize, _, _) = x
     (pSize, _, _) = p
     (lhs, rhs) = pivot p xs 
  
  (lhs, rhs) = pivot x xs 

-- Takes the first k elements from a list
takeFirstK :: Int -> [a] -> [a]
takeFirstK _ [] = []
takeFirstK k (x:xs)
 | k > 0 = x : takeFirstK (k-1) xs
 | otherwise = []

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Functions for printing the reslut --------------------------------------------------------------
---------------------------------------------------------------------------------------------------

-- Pad with space in front of string to make it length n
padLeft :: Int -> String -> String
padLeft n st
 | length st >= n = st
 | otherwise = padLeft n (' ' : st)

-- Turn list of Int:s into a string
myShowList :: [Int] -> String
myShowList [] = "[]"
myShowList (x:xs) = "[" ++ (show x) ++ hmyShowList xs
 where
  hmyShowList :: [Int] -> String
  hmyShowList [] = "]"
  hmyShowList (x:xs) = "," ++ (show x) ++ hmyShowList xs

-- Creates a line string of the sublist data 
strLine :: Int -> (Int, (Int, Int), [Int]) -> String
strLine n (size, (i, j), subList) = sizeS ++ pad ++ iS ++ pad ++ jS ++ pad ++ subListS
 where
  pad = "  "
  sizeS = padLeft n (show size)
  iS = padLeft n (show i)
  jS = padLeft n (show j)
  subListS = myShowList subList

-- Creates the result string for the k smallest sublist data
strLines :: Int -> [(Int, (Int, Int), [Int])] -> String
strLines padSize xs = (padLeft padSize "size") ++ pad ++ (padLeft padSize "i") ++ pad ++ (padLeft padSize "j") ++ pad ++ "sublist\n" ++ hstrLines xs
 where
  pad = "  "

  hstrLines :: [(Int, (Int, Int), [Int])] -> String
  hstrLines [] = ""
  hstrLines (x:xs) = strLine padSize x ++ "\n" ++ hstrLines xs 

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------
-- Final functions which computes the k smallest set and turns it into a String -------------------
--------------------------------------------------------------------------------------------------- 

smallestKset :: [Int] -> Int -> IO ()
smallestKset [] _ = error "smallestKset requiers nonempty list."
smallestKset xs k = putStr (input ++ result)
 where
  padSize = 4
  input = "\n" ++ "Entire list: " ++ myShowList xs ++ "\n\n"
  result = strLines padSize (takeFirstK k (quicksort (allSubArr xs)))

---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

