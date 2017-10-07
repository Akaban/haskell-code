main = readFile "triangle.txt" >>= print . solve . parse
parse = map (map read . words) . lines
solve = head . foldr1 step
step [] [z] = [z]
step (x:xs) (y:z:zs) = x + max y z : step xs (z:zs)
