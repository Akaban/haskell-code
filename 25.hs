next (i,(a,b)) = (i+1,(b,a+b))

digits :: Integer -> [Integer]
digits n = digitacc n []
    where digitacc 0 acc = acc
          digitacc x acc = digitacc q (r:acc)  
            where (q,r) = (x `div` 10,x `mod` 10)
main = do
let fibs = iterate next (0,(0,1))
putStrLn $ show $ (\(i,_) -> i) $ head $ dropWhile (\(i,(a,_)) -> length (digits a) < 1000) fibs




