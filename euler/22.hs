import Data.Char (ord,toLower)
import Data.List (sort)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy x xs = go x xs [] []
                where go _ [] acc1 acc2 = reverse (reverse acc1 : acc2)
                      go y (x:xs) acc1 acc2
                        | y == x = go y xs [] (reverse acc1 : acc2)
                        | otherwise = go y xs (x:acc1) acc2  
worth :: String -> Int
worth = foldl (\acc x -> (ord (toLower x) - 96) + acc) 0

main = do
 names <- readFile "p022_names.txt"
 let parsed = splitBy ',' names
     parsed1 = init parsed ++ ["ALONSO"]
     parsed2 = sort parsed1
 putStrLn . show $ let (_,r) = foldl (\(cpt,acc) x -> (cpt+1,acc + worth x * cpt)) (1,0) parsed2 in r
 
 
