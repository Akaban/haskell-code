power :: Integer -> Integer
power x = x ^ x

main = do
let result = sum $ map (power) [1..1000]
putStr . show $ result
