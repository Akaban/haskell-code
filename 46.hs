primes :: [Int] -> [Int]
primes [] = []
primes (x:xs) = x : primes [ y | y <- xs,y `rem` x /= 0]

lesserPrime :: Int -> [Int]
lesserPrime p = takeWhile (flip (<) p) (primes [2..])

isPrime :: Int -> Bool
isPrime x = [1,x] == [ y | y<-[1..x], x `rem` y == 0]

twiceSquare :: Int -> Bool
twiceSquare x = go x 1
    where go x n
            | 2 * n^2 == x = True
            | 2 * n^2 > x = False
            | otherwise = go x (n+1)

isSacre :: Int -> Bool
isSacre x = let prime = lesserPrime x
            in or $ do 
                        prim <- prime
                        let reste = x - prim
                        return $ twiceSquare reste

main = do
    let conjec = takeWhile (isSacre) $ filter (\x -> odd x && not (isPrime x)) [3..100000] 
    print conjec




