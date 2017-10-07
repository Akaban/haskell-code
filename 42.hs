-- n/2 * (n+1) 
-- n²/2 + n/2

import System.IO.Unsafe (unsafePerformIO)
import Data.Char (ord,toLower)

triangulars :: [Int]
triangulars = map (\n -> (n * n + n) `div` 2) [1..]

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy x xs = go x xs [] []
                where go _ [] acc1 acc2 = reverse (reverse acc1 : acc2)
                      go y (x:xs) acc1 acc2
                        | y == x = go y xs [] (reverse acc1 : acc2)
                        | otherwise = go y xs (x:acc1) acc2  

isTriangular :: Int -> Bool
isTriangular n = dicho n triangulars
    where dicho n (x:xs)
            | x > n = False
            | n == x = True
            | otherwise = dicho n xs

worth :: String -> Int
worth = foldl (\acc x -> (ord (toLower x) - 96) + acc) 0



main = do
    f <- readFile "42.txt"
    let s = (init (splitBy ',' f)) ++ ["YOUTH"]
        s1 = filter (isTriangular . worth) s
    print s1
    print $ length s1
    
