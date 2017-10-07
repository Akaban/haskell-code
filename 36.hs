nbaseOfDecimal :: Int -> Int -> [Int]
nbaseOfDecimal n d = go d []
    where go d acc
            | d == 0 = acc
            | otherwise = let (q,r) = (d `div` n,d `rem` n) in go q (r:acc)

digitss = nbaseOfDecimal 10
binary = nbaseOfDecimal 2

assert x = let d = digitss x
               b = binary x
           in d == reverse d && b == reverse b

main = do
    let a = filter assert [1..1000000]
    print . show . sum $ a
