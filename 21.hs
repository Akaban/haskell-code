divisors :: Int -> [Int]
divisors k = divisors' 2 k
    where divisors' n k | n*n > k = [1]
                        | n*n == k = [1,n]
                        | k `mod` n == 0 = (n:(k `div` n):result)
                        | otherwise = result
                            where result = divisors' (n+1) k

df :: Int -> Int
df = sum . divisors

isAmicable :: Int -> Bool
isAmicable a = let b = df a in df b == a && a /= b
