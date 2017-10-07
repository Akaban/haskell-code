import Data.Set (union)

primesTME = 2 : _Y ((3:) . gaps 5 . joinT . map (\p-> [p*p, p*p+2*p..]))
 
joinT ((x:xs):t) = x : (union xs . joinT . pairs) t  -- set union, ~=
  where  pairs (xs:ys:t) = union xs ys : pairs t     --    nub.sort.concat
 
gaps k s@(x:xs) | k < x = k:gaps (k+2) s       -- ~= [k,k+2..]\\s, 
                | True  =   gaps (k+2) xs      --   when null(s\\[k,k+2..])
