import Control.Monad

digitString :: [Int] -> String
digitString = concat . map show

selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs) : [ (y,x:ys) | (y,ys) <- selections xs ]

permut xs = [ y:zs | (y,ys) <- selections xs, zs <- permut ys]
