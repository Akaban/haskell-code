import System.IO.Unsafe (unsafePerformIO)
import qualified Data.List as L

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy x xs = go x xs [] []
                where go _ [] acc1 acc2 = reverse (reverse acc1 : acc2)
                      go y (x:xs) acc1 acc2
                        | y == x = go y xs [] (reverse acc1 : acc2)
                        | otherwise = go y xs (x:acc1) acc2  
grid :: [[Int]]
grid = 
    let raw = unsafePerformIO $ readFile "11grid.txt"
        parse1 = lines raw
        parse2 = map (splitBy ' ') parse1
    in map (\x -> map read x) parse2


possibilities grid = 
    let max_X = length $ grid !! 0
        max_Y = length grid
        getAdjacent grid x y =
            let direction4 f = filter (\(x,y) -> x >= 0 && x < max_X && y >= 0 && y < max_Y) . take 4 $ iterate f (x,y)
            in filter (\x -> length x == 4) $ direction4 (\(x,y) -> (x+1,y)) : direction4 (\(x,y) -> (x,y-1)) : direction4 (\(x,y) -> (x-1,y-1)) : 
                                                                                   direction4 (\(x,y) -> (x-1,y)) : direction4 (\(x,y) -> (x,y+1)) : [direction4 (\(x,y) -> (x-1,y+1))]
        allP = do 
            y <- [0..(max_Y-1)]
            x <- [0..(max_X-1)]
            getAdjacent grid x y
        allP1 = map (\l -> map (\(x,y) -> grid !! x !! y) l) allP
    in L.sortBy (flip compare) . map (\x -> (product x,x)) $ allP1

