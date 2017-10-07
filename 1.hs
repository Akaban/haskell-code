multiplesOf3or5 m = [ x | x<- [1..m-1], x `mod` 3 == 0 || x `mod` 5 == 0]

main = do
    let result = sum $ multiplesOf3or5 1000
    putStrLn $ "result is " ++ show result
