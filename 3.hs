primes m = let maxi = (fromIntegral . floor . sqrt) m in sieves [2..maxi]
        where sieves [] = [] 
              sieves (p:xs) = p : sieves [ x | x<-xs, x `mod` p /= 0]

primesFactor n = [ x | x<-primes n, n `mod` x == 0 ]

maximum' []  = error "oklm"
maximum' [x] = x
maximum' (x:xs) =
    if x > maximum xs then x
    else maximum xs
