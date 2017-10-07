import System.IO.Unsafe (unsafePerformIO)

-- sum of all positive integer that cannot be writter
-- as the sum of two abundant number
-- abundant x = sum proper divisors x > x


divisors :: Int -> [Int]
divisors k = divisors' 2 k
    where divisors' n k | n*n > k = [1,k]
                        | n*n == k = [1,n,k]
                        | k `mod` n == 0 = (n:(k `div` n):result)
                        | otherwise = result
                            where result = divisors' (n+1) k

properDivisors x = filter (flip (/=) x) . divisors $ x


isAbundant x = ((<) x) . sum . properDivisors $ x

abundants :: [Int]
abundants = read . unsafePerformIO $ readFile "abundants.txt"

testWritten x =
    let rg = takeWhile (\a -> a < x) abundants
    in [ (a,b) | a<-rg, b<-rg, a+b == x] 

main = do
    let a = filter (null . testWritten) [24..28123] 
    print . sum $ a

