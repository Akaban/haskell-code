import Data.List (sortBy)
-- a b c integer
-- a + b + c = 1000
-- c = 1000 - a -b
-- a + b < 1000
-- a^2 + b^2 = c^2

-- p = (2k+1)
-- c = 2k+1-a-b
--

doRecurse p = recurseFind 2 1 []
 where d = p `div` 2
       recurseFind a b sol
        | a == d = sol
        | b > d = recurseFind (a+1) 1 sol
        | a + b + c == p && a^2 + b^2 == c^2 = recurseFind (a+1) 1 ((a,b,c):sol)
        | b >= p = recurseFind (a+1) 1 sol
        | otherwise =  recurseFind a (b+1) sol
            where c = p - a - b
main = do
    let a = map (\x -> (x,length (doRecurse x))) [ x | x<-[1..1000], even x]
    print . sortBy (\(_,i) (_,j) -> compare i j) $ a

-- a + c > p
-- a + p - a - b > p
-- p - b > p
