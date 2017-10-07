import qualified Data.Foldable as F
import Data.Monoid
import Data.List
import System.Random
import System.IO.Unsafe (unsafePerformIO)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show,Eq,Ord)
type TreeInt = Tree Int

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node lt v rt) = Node (fmap f lt) (f v) (fmap f rt)

instance F.Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node lt v rt) = F.foldMap f lt `mappend`
                             f v `mappend`
                             F.foldMap f rt

height :: Tree a -> Int
height Leaf = 0
height (Node l _ r) = 1 + max (height l) (height r) 

fromJust :: Maybe a -> Maybe a -> Maybe a
fromJust Nothing Nothing = Nothing
fromJust Nothing (Just x) = Just x
fromJust (Just x) Nothing = Just x
fromJust a _ = a

maxMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
maxMaybe (Just x) (Just y) = if x >= y then Just x else Just y
maxMaybe a b = fromJust a b

minMaybe :: (Ord a) => Maybe a -> Maybe a -> Maybe a
minMaybe (Just x) (Just y) = if x <= y then Just x else Just y
minMaybe a b = fromJust a b

maxTree :: (Ord a) => Tree a -> a
maxTree t = case maxV of
              Just x -> x
              Nothing -> error "maxTree with empty tree"
  where maxV = F.foldl (\acc x -> maxMaybe (Just x) acc) Nothing t

minTree :: (Ord a) => Tree a -> a
minTree t = case minV of
              Just x -> x
              Nothing -> error "minTree with empty tree"
  where minV = F.foldl (\acc x -> minMaybe (Just x) acc) Nothing t

toList :: Tree a -> [a]
toList = F.foldMap return

insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree Leaf v = Node Leaf v Leaf
insertTree (Node lt x rt) v
  | v >= x = Node lt x (insertTree rt v)
  | otherwise = Node (insertTree lt v) x rt

isSearchTree :: (Ord a) => Tree a -> Bool
isSearchTree Leaf = True
isSearchTree (Node lt v rt) = validateLeft v lt && validateRight v rt && isSearchTree lt && isSearchTree rt
  where validateLeft v Leaf = True
        validateLeft v (Node _ x _) = x < v
        validateRight v Leaf = True
        validateRight v (Node _ x _) = x >= v 

mirror :: Tree a -> Tree a
mirror Leaf = Leaf
mirror (Node l v r) = Node (mirror r) v (mirror l)

makeTree :: (Ord a) => [a] -> Tree a
makeTree [] = Leaf
makeTree (x:xs) =
  let lesser = filter (\y -> y < x) xs
      bigger = filter (\y -> y > x) xs
  in Node (makeTree lesser) x (makeTree bigger)

randomIntList :: Int -> (Int,Int) -> StdGen -> [Int]
randomIntList n range = take n . unfoldr (Just . randomR range)

randomListHelper x range = unsafePerformIO $
  do
    seed <- newStdGen
    let l = randomIntList x range seed
    return l

randomTreeInt :: Int -> (Int,Int) -> TreeInt
randomTreeInt height = makeTree . randomListHelper (2^height)

treeEquiv :: (Eq a) => Tree a -> Tree a -> Bool
treeEquiv t1 t2 = t1==t2 || t1==(mirror t2)


