--Pile codée avec des entiers

type Pile = Int

pileVide :: Int
pileVide = 0

estVide :: Pile -> Bool
estVide = (==) 0

ajouter :: Pile -> Int -> Pile
ajouter x p = p * 100 + x

depiler :: Pile -> Int
depiler p = p `div` 100

tete :: Pile -> Int
tete p = p `mod` 100

unstack :: Pile -> (Int,Pile)
unstack p = (p `mod` 100,p `div` 100)

unfoldStack :: Pile -> [Int]
unfoldStack 0 = []
unfoldStack x = let (r,d) = unstack x in r : unfoldStack d 

foldToStack :: [Int] -> Pile
foldToStack xs = let (stack,_) = foldr (\x (a,c) -> (x * (100 ^ (c-1)) + a,c+1)) (0,1) xs in stack
