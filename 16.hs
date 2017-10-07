digits :: Integer -> [Integer]
digits n = digitacc n []
    where digitacc 0 acc = acc
          digitacc x acc = digitacc q (r:acc)  
            where (q,r) = (x `div` 10,x `mod` 10)

main = do
    let n = 2 ^ 1000
        d = digits n
        s = sum d
    putStrLn $ show s
