import Data.List

assertMultiple n = any (\xs -> product xs == n) mult 
                   where mult = combinations [ x | x <- [100..999] , mod n x == 0]

isPalindrom :: Int -> Bool
isPalindrom x = xs == reverse xs
    where xs = show x


combinations ary = nub . map (sort) $ mapM (const ary) [1..2]


pal = [ x | x <- [1..998001], isPalindrom x && assertMultiple x]

