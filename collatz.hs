collatz :: Int -> [Int]
collatz n = docollatz n acc
    where docollatz n acc 
        | n == 1 = 1 : acc
        | even n = docollatz en (en:acc)
        | otherwise = dollatz on (on:acc)
            where en,on = en/2,
