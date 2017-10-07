import qualified Data.List as L

type Reserve = [(Int,Int)]

delete :: Eq a => a -> [a] -> [a]
delete x (y:ys) = if x==y then ys else y : delete x ys

uniq :: Eq a => [a] -> [a]
uniq = foldl (\acc x -> if x `elem` acc then acc else x:acc) []

howMuch :: [Int] -> Reserve
howMuch xs = map (\x -> (head x,length x)) $ L.group (L.sort xs)

reduce :: Reserve -> Int -> Reserve
reduce [] _ = []
reduce ((x,n):xs) r
    | x == r = if n <= 1 then xs else (x,n-1) : xs
    | otherwise = (x,n) : reduce xs r

combinations :: Int -> [a] -> [[a]]
combinations n (x:xs) = [x:comb | comb <- combinations (n-1) xs]
      ++ combinations n xs


--findWriting :: Int -> [Reserve]
--findWriting s = let r = [howMuch . replicate s $ 1] in go r acc
--    where go r acc
