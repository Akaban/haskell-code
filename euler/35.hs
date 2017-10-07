import Data.Numbers.Primes

permut :: Eq a =>  [a] -> [[a]]
permut xs = go xs [] 0
    where long = length xs
          go ys acc cpt = if cpt == long then acc
                          else let lst = last ys
                                   liste = lst : init ys
                               in go liste (liste:acc) (cpt+1)

isPrime' :: Int -> Bool
isPrime' x = [1,x] == [ y | y<-[1..max], x `rem` y == 0]
    where max = truncate . sqrt . fromIntegral $ x

isCircular :: Int -> Bool
isCircular = all (isPrime . read) . permut . show


main = do
    let app = takeWhile (flip (<) 1000000) primes
        res = filter (isCircular) app
    print . length $ res
    print res
