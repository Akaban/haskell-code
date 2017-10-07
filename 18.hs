import System.IO.Unsafe (unsafePerformIO)

data Tree2 a = Node a (Tree2 a) (Tree2 a) | Leaf deriving (Eq,Show)
data Direction = L | G
type Path = [Direction]

maxPath :: Tree2 Int -> Int
maxPath (Node x lt rt) = x + max (maxPath lt) (maxPath rt)
maxPath Leaf = 0

a :: [[Int]]
a = [[3],[7,4],[2,4,6],[8,5,9,3]]

testTree :: Tree2 Int
testTree = Node 3 (Node 7 (Node 2 (Node 8 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 4 (Node 5 Leaf Leaf) (Node 9 Leaf Leaf))) (Node 4 (Node 4 (Node 5 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 6 (Node 9 Leaf Leaf) (Node 3 Leaf Leaf)))

firstTriangle :: [[Int]] -> [[Int]]
firstTriangle [] = []
firstTriangle (_:r) = map (init) r 

secondTriangle :: [[Int]] -> [[Int]]
secondTriangle [] = []
secondTriangle (_:r) = map (tail) r

toTree :: [[Int]] -> Tree2 Int
toTree [] = Leaf
toTree a@(top:xs) = Node x (toTree f) (toTree s)
   where f = firstTriangle a
         s = secondTriangle a
         x = head top

pyramide :: FilePath -> String
pyramide fpath = unsafePerformIO $ readFile fpath

toMatrix :: String -> [[Int]]
toMatrix s = map (map read) w
   where p = lines s
         w = map (words) p

fdp :: Tree2 Int -> Tree2 Int
fdp (Node x (Node y la lb) (Node z lf lg)) = if y > z then (Node (x+y) la lb) else (Node (x+z) lf lg)
fdp (Node x la lb) = Node x Leaf Leaf
fdp Leaf = Leaf

fdp2 (Node x (Node y Leaf Leaf) (Node z Leaf Leaf)) = Node (x + max y z) Leaf Leaf
fdp2 (Node x a b) = Node x (fdp2 a) (fdp2 b)

fdp3 :: Tree2 Int -> Tree2 Int
fdp3 (Node x la lb) = let (Node a b c) = fdp3 la
                          (Node d e f) = fdp3 lb
                      in Node (x + max a d) Leaf Leaf
fdp3 Leaf = Leaf

suma :: Num a => Tree2 a -> a
suma (Node x y z) = x + suma y + suma z
suma Leaf = 0

applyFdp :: Tree2 Int -> Tree2 Int
applyFdp a@(Node x Leaf Leaf) = a
applyFdp a@(_) = applyFdp $ fdp2 a 

reverseTree (Node x y z) = Node x z y
reverseTree Leaf = Leaf

profondeur :: Tree2 Int -> Int
profondeur (Node x a b) = 1 + max (profondeur a) (profondeur b)
profondeur Leaf = 0
