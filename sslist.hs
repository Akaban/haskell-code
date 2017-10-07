addall x = map ((:) x)

sous [] = [[]]
sous (x:xs) = let p = sous xs in addall x p

ajout_element :: a -> [[a]] -> [[a]]
ajout_element e = foldl (\acc x -> (e:x):acc) []


rev_append :: [a] -> [a] -> [a]
rev_append l1 l2 = foldl (\acc x -> x:acc) l2 l1

sublist_trec :: [a] -> [[a]]
sublist_trec = foldl (\acc x -> let acc' = ajout_element x acc in rev_append acc' acc) [[]] 

map_trec :: (a -> b) -> [a] -> [b]
map_trec f = foldl (\acc x -> f x : acc) [] 
