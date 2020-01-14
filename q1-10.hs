-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast xs
  | length xs == 2 = head xs
  | otherwise = myButLast (tail xs)

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt xs 0 = head xs
elementAt xs n = elementAt (tail xs) (n-1)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Pronlem 5
myReverse :: [a] -> [a]
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs
  | (head xs) == (last xs) = isPalindrome (init(tail xs))
  | otherwise = False

-- Problem 7
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten(List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = [x] ++ (compress (filter (/=x) xs))

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = [filter (==x) (x:xs)] ++ pack (filter (/=x) xs)

--Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)