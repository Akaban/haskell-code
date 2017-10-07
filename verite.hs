import Data.List
import Data.Maybe

type FormuleS = String
data Formule  = AND Formule Formule | OR Formule Formule | IMP Formule Formule | NOT Formule | A AtomicF
data AtomicF  = VAR Char | TOP | BOTTOM | EMPTY deriving (Eq,Ord)
type Interpretation = [(AtomicF,Bool)] 
newtype TableVerite = TableVerite { getTable :: (Formule,[(Interpretation,Bool)]) } deriving Show

showCharr :: Char -> String
showCharr c = [c]

instance (Show Formule) where
   show = let showF p (AND f1 f2) = (if p then "(" else "") ++ showF True f1 ++ "/\\" ++ showF True f2 ++ (if p then ")" else "")
              showF p (OR f1 f2)  = (if p then "(" else "") ++ showF True f1 ++ "\\/" ++ showF True f2 ++ (if p then ")" else "")
              showF p (IMP f1 f2) = (if p then "(" else "") ++ showF True f1 ++ "=>" ++ showF True f2 ++ (if p then ")" else "")
              showF p (NOT f1) = "!(" ++ show f1 ++ ")"
              showF p (A a) = show a
          in showF False
instance (Eq Formule) where
   (==) f1 f2 = let tv1 = calculateTable f1
                    tv2 = calculateTable f2
                in tv1 == tv2

instance (Show AtomicF) where
   show (VAR c) = showCharr c
   show TOP = "TOP"
   show BOTTOM = "BOTTOM"
   show EMPTY = ""

instance (Eq TableVerite) where
   (==) (TableVerite (_,t1)) (TableVerite (_,t2)) = t1 == t2

vp :: Char -> Formule
vp c = A (VAR c)

pOR :: Char -> Char -> Formule
pOR a b = OR (vp a) (vp b)

pAND :: Char -> Char -> Formule
pAND a b = AND (vp a) (vp b)

conj :: [Formule] -> Formule
conj [f] = f
conj (f:fs) = AND f (conj fs)

disj :: [Formule] -> Formule
disj [f] = f
disj (f:fs) = OR f (disj fs)

sequent :: [Formule] -> [Formule] -> Formule
sequent hyp conc = IMP (conj hyp) (disj conc)

--Fonction de substitution
substitute :: Formule -> AtomicF -> AtomicF -> Formule
substitute (AND f1 f2) r1 r2 = AND (substitute f1 r1 r2) (substitute f2 r1 r2)
substitute (OR f1 f2) r1 r2 = OR (substitute f1 r1 r2) (substitute f2 r1 r2)
substitute (IMP f1 f2) r1 r2 = IMP (substitute f1 r1 r2) (substitute f2 r1 r2)
substitute (NOT f1) r1 r2 = NOT (substitute f1 r1 r2)
substitute ori@(A (VAR c)) (VAR r) r2 = if c==r then A r2 else ori
substitute _ _ r = A r

-- Elimine les doublons dans une liste
uniq :: (Eq a) => [a] -> [a]
uniq = foldr (\x acc -> if x `elem` acc then acc else x:acc) []

-- Rends les variables utilisées dans une formule
vars :: Formule -> [AtomicF]
vars (AND f1 f2) = vars f1 ++ vars f2
vars (OR f1 f2)  = vars f1 ++ vars f2
vars (IMP f1 f2) = vars f1 ++ vars f2
vars (NOT f1)    = vars f1
vars (A (VAR a)) = [VAR a]
vars _           = []

varsUniq = sort . uniq . vars

-- Prends une formule et une fonction d'interpretation et rends sa valeur de vérité
val :: (AtomicF -> Bool) -> Formule -> Bool
val i (AND f1 f2) = if val i f1 then val i f2 else False
val i (OR f1 f2)  = if val i f1 then True else val i f2
val i (IMP f1 f2) = if val i f1 then val i f2 else True
val i (NOT f1)    = not (val i f1)
val i (A a) = i a
               
-- Rends une fonction d'interpretation d'après un dictionnaire d'interpretation
genInterp :: [(AtomicF,Bool)] -> (AtomicF -> Bool)
genInterp values v@(VAR c) = fromMaybe False $ lookup v values 
genInterp _ TOP = True
genInterp _ BOTTOM = False
genInterp _ EMPTY = True

--Prends une formule et génère toutes les interpretations possibles
allInterp :: Formule -> [[(AtomicF,Bool)]]
allInterp f = let var = varsUniq f
                  possibilities = map (\x -> [(x,True),(x,False)]) var
              in sequence possibilities

-- Calcule le table de vérité d'une formule
calculateTable :: Formule -> TableVerite
calculateTable formule = let possib = allInterp formule
                             res    = do
                                       interp <- possib
                                       let interpF = genInterp interp
                                       return (interp,val interpF formule)
                         in TableVerite (formule,res)

-- Affiche la table de vérité
printTableVerite :: TableVerite -> IO ()
printTableVerite (TableVerite (f,res)) =
   let boolS b = if b then 'V' else 'F'
       seqLigne = do
         (arg,val) <- res
         let calcSeq (x:xb) = putChar (boolS x) : putStr " " : calcSeq xb
             calcSeq []     = [putStr " ||  ",putChar (boolS val),putStr "\n"]
         calcSeq (map (\(_,b) -> b) arg)
    in do
        let vars = varsUniq f 
        mapM (\(VAR c) -> do {putChar c;putStr " "})  vars
        putStr " ||  "
        print f
        sequence_ seqLigne
        return ()
   
tv = printTableVerite . calculateTable

f2 :: Formule
f2 = OR (AND (vp 'P') (vp 'Q')) (AND (vp 'P') (vp 'R'))


