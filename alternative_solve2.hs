import Data.List
import System.IO

--Renvoie tout les diviseurs d'un entier donné
divisorsOf :: (Integral a) => a -> [a]
divisorsOf 0 = error "Division by zero"
divisorsOf x = [ n | n<-[1..x], x `rem` n == 0]

--Renvoie la liste d'entiers avec leur opposés
addNegative :: (Num a) => [a] -> [a]
addNegative = concatMap (\x -> [x,-x])  

type Poly    = [Float]
type PolyInt = [Integer]

power :: (Num a,Eq a,Integral b) => a -> b -> a
power x 0 = 1
power x 1 = x
power 1 y = 1
power x y = power' x y 1
   where power' x y acc
            | y == 0    = acc
            | otherwise = power' x (y-1) (acc*x)

--Renvoie une liste avec tout les monomes du polynome evalués en un point
calculatePolyMonoms ::  PolyInt -> Integer -> PolyInt
calculatePolyMonoms [] _ = [] 
calculatePolyMonoms all@(mon:poly) x = monomsPoly 
   where monomsPoly = (mon*(x `power` degre)) : (calculatePolyMonoms poly x)
         degre = fromIntegral $ (length $ dropWhile(==0) all) - 1 

--Evalue le polynome
evalPoly poly x = sum $ calculatePolyMonoms poly x

uniq :: (Eq a) => [a] -> [a]
uniq = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

-- Sépare les éléments d'indice pair des éléments d'indice impair
separate :: (Eq a) =>  [a] -> ([a],[a])
separate xs = separate' xs ([],[])
   where separate' (x:xs) (a1,a2) = if xs == [] then (a1 ++ [x],a2) else separate'' xs (a1 ++ [x],a2)
         separate'' (x:xs) (a1,a2) = if xs == [] then (a1,a2 ++ [x]) else separate' xs (a1,a2 ++ [x])

separate2 :: [a] -> ([a],[a])
separate2 xs = separate2' xs ([],[])
   where separate2' [] acc = acc
         separate2' (x:xs) (a,b) = let na = x:a in separate2' xs (b,na)

--Renvoie vrai si les monomes sont de signes alternés
alternativeSigns :: [Integer] -> Bool
alternativeSigns xs
   | (head x1) > 0 = all (>0) x1 && all (<0) x2
   | (head x1) < 0 = all (<0) x1 && all (>0) x2
   | otherwise     = alternativeSigns . simplifyPoly $ xs
      where (x1,x2) = separate xs 

simplifyPoly :: PolyInt -> PolyInt
simplifyPoly = dropWhile (==0)

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
showPoly poly 
   | mon > 0   = ("+" ++ show mon ++ "*x^" ++ show degre) ++ showPoly pol
   | mon < 0   = (show mon ++ "*x^" ++ show degre) ++ showPoly pol
   | otherwise = "" ++ showPoly pol
      where all@(mon:pol) = simplifyPoly poly
            degre     = (length all) - 1

-- Ne garde que les supposées racines parmis les diviseurs du dernier monome
purify :: [Integer] -> PolyInt -> [Integer]
purify racines pol = [ x | x<-racines, (evalPoly pol x) == 0]

-- Factorise le polyome par x et renvoie le polynome ainsi créé
factorX :: PolyInt -> PolyInt
factorX = init

--Fonctions permettant d'extraire les données nécessaire à la résolution du polynome (racines supposées,somme des racines, produit des racines)
racinesProbPoly :: PolyInt -> (Integer,Integer,Integer,[Integer])
racinesProbPoly [] = error "empty polynom"
racinesProbPoly poly
   | alternativeSigns pol     = (degre,negate monom2,c,purify (divisorsOf cpos) poly)
   | (filter (>0) pol) == pol = (degre,negate monom2,c,purify (map (negate) ( divisorsOf cpos)) poly)
   | otherwise                = (degre,negate monom2,c,purify (addNegative (divisorsOf cpos)) poly)
      where pol    = simplifyPoly poly
            monom2 = pol !! 1
            degre  = toInteger $ (length pol) - 1
            l      = last pol
            cpos   = abs l
            c      = if odd degre then (negate l) else l
                        
-- Elimine les racines superflues et ne garde que le n-uplet qui annule le polynome
eliminateRacines :: (Integer,Integer,Integer,[Integer]) -> [Integer]
eliminateRacines raw = 
   let (degreInt,sumMon,productMon,racines) = raw 
       degre = fromIntegral degreInt 
       combinaisons = combinationsPoly degre racines  
   in  head [xs | xs<-combinaisons, (product xs) == productMon && (sum xs) == sumMon]
                          
-- Résouds un polynome donné
solvePoly :: PolyInt -> [Integer]
solvePoly poly = if (last poly == 0) then 0 : racines --Si le dernier monome est nul alors on prends soin de l'ajouter aux racines (il serait omis sinon)
                 else eliminateRacines prob -- Sinon on applique le procedé normal
   where prob  = racinesProbPoly poly
         racines = eliminateRacines . racinesProbPoly $ factorX poly -- factorX factorise le polynome par x (on retire un 0 de la liste des racines)

--Fonction de combinaison
combinationsPoly :: Int -> PolyInt -> [[Integer]]
combinationsPoly x poly = uniq . map (sort) $ mapM (const poly) [1..x]
   
readArray :: [String] -> [Integer]
readArray [] = []
readArray (x:xs) = read x : readArray xs

main = do
putStrLn "Entrer les monomes du polynome un par un puis faire ctrl+D"
contents <- getContents

let polyString = lines contents
    polyInt = readArray polyString
    racines = solvePoly polyInt
    racinesString = map (show) racines

putStrLn ("Resolution du polynome " ++ (showPoly polyInt))
putStrLn ("Les racines sont " ++ (intercalate "," racinesString))
