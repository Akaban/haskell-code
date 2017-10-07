digits :: Int -> [Int]
digits n = digitacc n []
    where digitacc 0 acc = acc
          digitacc x acc = digitacc q (r:acc)  
            where (q,r) = (x `div` 10,x `mod` 10)

factorial :: Int -> Int
factorial n = if n==0 then 1 else facacc n 1
    where facacc n acc
            | n == 1 = acc
            | otherwise = facacc (n-1) (acc*n)

factorials :: [Int]
factorials = map factorial [0..9]

isSacrifice :: Int -> Bool
isSacrifice n = let d = digits n in if sum d <= 10 then False else foldl (\acc x -> factorials !! x + acc) 0 d == n

main = do
    print $ filter (isSacrifice) [145..]
