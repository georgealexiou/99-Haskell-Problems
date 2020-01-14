-- Problem 11
data Encoding a = Multiple Int a | Single a deriving (Show)

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map h . encode
  where
      h (1,x) = Single x
      h (n,x) = Multiple n x

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = [filter (==x) (x:xs)] ++ pack (filter (/=x) xs)

-- Problem 12
decodeModified :: Show a => [Encoding a] -> [a]
decodeModified [Single x] = [x]
decodeModified [Multiple n x] = dup x n
  where
		dup :: a -> Int -> [a]
		dup y 0 = []
		dup y m = y : dup y (m-1)
decodeModified (x:xs) = decodeModified [x] ++ decodeModified xs

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 15
repli :: [a] -> Int -> [a]
repli xs n = concat [repeat1 x n | x <- xs]

repeat1 :: a -> Int -> [a]
repeat1 x 0 = []
repeat1 x n = x : repeat1 x (n-1)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery list count = h list count count
  where h [] _ _ = []
        h (x:xs) count 1 = h xs count count
        h (x:xs) count n = x : (h xs count (n - 1))

-- Problem 17
split :: [a] -> Int -> ([a],[a])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs a b = reverse (drop (length xs - (b)) (reverse (drop (a-1) xs)))

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate xs n
  | n == 0 = xs
  | n > 0 = drop n xs ++ take n xs
  | n < 0 = reverse (take n (reverse xs)) ++ reverse (drop n (reverse xs))

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)