import Data.Numbers.Primes
import Data.List (tails)

truncated :: Int -> ([Int],[Int])
truncated y =
    let x = show y
        t = map read . init . tails $ x
    in (t,init . map (read . reverse) . tails . reverse $ x)

isTruncatablePrime x =
    let (a,b) = truncated x
    in all (isPrime) a && all (isPrime) b
