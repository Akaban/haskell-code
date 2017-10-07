findMax :: Int -> (Int,Int,Int)
findMax max = go 1 2
    where go a b
            | a == max = error "Not found"
            | b == max = go (a+1) 2
            | b < a = go a (b+1)
            | a^2 + b^2 == c^2 = (a,b,c)
            | otherwise = go a (b+1)
                where c=max-a-b
    
