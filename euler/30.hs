digits :: Int -> [Int]
digits n = digitacc n []
    where digitacc 0 acc = acc
          digitacc x acc = digitacc q (r:acc)  
            where (q,r) = (x `div` 10,x `mod` 10)

powerDigit :: Int -> Int -> Bool
powerDigit di n = let d = digits di
                 in di == (sum $ map (flip (^) n) d)
