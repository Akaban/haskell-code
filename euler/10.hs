primes :: [Int] -> [Int]
primes [] = []
primes (x:xs) = x : primes [ y | y <- xs,y `rem` x /= 0]

reverseAppend :: [a] -> [a] -> [a]
reverseAppend xs ys = foldl (flip (:)) ys xs

primesOpti :: Int -> [Int]
primesOpti max = go 2 []
    where go n acc
            | n > max = acc
            | otherwise = go (n+100) (reverseAppend (primes [n..(n+100)]) acc)

isDivisible :: Int -> Int -> Bool
isDivisible x y = x `mod` y == 0

notDivisible :: Int -> Int -> Bool
notDivisible x y  = not $ isDivisible x y


erathos :: Int -> [Int]
erathos max = go [2..max]
    where go (x:xs)
            | x^2 > max = (x:xs)
            | otherwise = x : go (filter (flip notDivisible x) xs)


main = do
    let p = erathos 2000000
        sum = foldl1 (+) p
    putStrLn $ show sum
