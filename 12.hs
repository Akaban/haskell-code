divisors :: Int -> [Int]
divisors k = divisors' 2 k
    where divisors' n k | n*n > k = [k]
                        | n*n == k = [n,k]
                        | k `mod` n == 0 = (n:(k `div` n):result)
                        | otherwise = result
                            where result = divisors' (n+1) k

triangle :: Int -> Int
triangle n = sum [1..n]

main = do
    let result = head $ dropWhile ((flip (<) 500) . length . divisors ) (map triangle [1..])
    putStrLn $ show result
