import Data.List

type PolyInt = [Integer]
data MonomX = M Integer Integer deriving (Show,Ord,Eq)
type PolynomX = [MonomX]

-- Transforme un PolyInt en PolynomX
fromPolyInt :: PolyInt -> PolynomX
fromPolyInt xs = let (res,cpt) = foldl (\(p,d) x -> ((M x d : p), (d+1))) ([],0) xs in res


-- Applique une fonction sur un polynôme
polyFold :: (Integer -> Integer -> Integer) -> PolynomX -> PolynomX -> PolynomX
polyFold f xs ys = loop xs ys
    where loop [] ys = ys
          loop xs [] = xs
          loop (M c0 e0:xs) (M c1 e1:ys) =
            case (e0 `compare` e1) of
                LT -> M c0 e0 : loop xs (M c1 e1:ys)
                EQ -> let c' = f c0 c1 in M c' e0 : loop xs ys
                GT -> M c1 e1 : loop (M c0 e0:xs) ys

--Ajoute deux polynomes
addPol = polyFold (+)

-- Multiplie un monome par un scalaire
mulS :: MonomX -> Integer -> MonomX
mulS (M c e) k = M (c*k) e

--Multiplie deux monomes
mulM :: MonomX -> MonomX -> MonomX
mulM (M c0 e0) (M c1 e1) = M (c0*c1) (e0+e1)

--Multiplie un polynome par un monome
mulPM :: PolynomX -> MonomX -> PolynomX
mulPM p m = map (mulM m) p

--Multiplie deux polynomes
mulP :: PolynomX -> PolynomX -> PolynomX
mulP [] _ = []
mulP _ [] = [] 
mulP (m1:p1) (m2:p2) = mulM m1 m2 : addPol (mulPM p2 m1) (mulP (m2:p2) p1)  

-- Transforme le polynome en chaine de caractere
showPoly :: PolyInt -> String
showPoly [] = ""
showPoly [x]
   | x > 0     = "+" ++ show x
   | x == 0    = ""
   | otherwise = show x
showPoly (x:[xy])
   | x > 0     = "+" ++ show x ++ "x" ++ showPoly [xy]
   | x == 0    = "" ++ showPoly [xy]
   | otherwise = show x ++ "x" ++ showPoly [xy]
showPoly poly = showPolrec poly deg
        where deg = length poly - 1
              showPolrec poly d
                    | d < 2    = showPoly poly
                    | mon > 0   = ("+" ++ show mon ++ "x^" ++ show d) ++ showPolrec pol (d-1)
                    | mon < 0   = (show mon ++ "x^" ++ show d) ++ showPolrec pol (d-1)
                    | otherwise = "" ++ showPolrec pol (d-1)
                        where mon:pol = poly

zeroPoly :: Int -> PolyInt
zeroPoly = flip replicate 0

convert :: PolyInt -> Int -> PolyInt
convert xs n
   | taille > n = drop diff xs
   | taille < n = zeroPoly diff ++ xs
   | taille == n = xs
      where taille = length xs
            diff   = abs (taille-n)

sumPoly :: PolyInt -> PolyInt -> PolyInt
sumPoly p q 
   | degq == degp = zipWith (+) p q
   | otherwise = let (np,nq) = (convert p maxdeg,convert q maxdeg) in zipWith (+) np nq
      where degp = length p
            degq = length q
            maxdeg = max degp degq 

simplifyPoly :: PolyInt -> PolyInt
simplifyPoly = dropWhile (==0)


addIndex :: PolyInt -> [(Integer,Integer)]
addIndex xs = zipWith (,) xs [0..taille]
            where taille = fromIntegral . length $ xs

-- Multiplie deux polynômes
prodPoly :: PolyInt -> PolyInt -> PolyInt
prodPoly p q = let (np,nq) = (addIndex p,addIndex q)  
                    in let r = [ (cp*cq,ip+iq) | (cp,ip) <- np, (cq,iq) <- nq]
                    in let sort  = sortBy (\(_,i) (_,j) -> i `compare` j) r
                           group = groupBy (\(_,i) (_,j) -> i==j) sort
                  in map sum $ map (map (\(x,_) -> x)) group        

main = do
let p = [1..200]
    pX = fromPolyInt p
    in let x = prodPoly p p
    in print x    
