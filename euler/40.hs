import Data.Char (ord,digitToInt)

sacre :: String
sacre = concat [show n | n <- [1..]]

d :: Int -> Int
d x = digitToInt (sacre !! (x-1))

a = [1,10,100,1000,10000,100000,1000000]

main = do
let result = product $ map d a
putStrLn . show $ result
