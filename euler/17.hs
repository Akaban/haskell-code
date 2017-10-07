chiffres,dizaine1 :: [String]
chiffres = ["one","two","three","four","five","six","seven","eight","nine"]
dizaine1 = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
dizaine2 = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

intWord :: Int -> String
intWord x | x < 10 = chiffres !! (x-1)
          | x < 20 = dizaine1 !! (x-10)
          | x < 100 = let (q,r) = (x `div` 10,x `rem` 10) in 
                        if r == 0 then dizaine2 !! (q - 2)
                        else (dizaine2 !! (q - 2)) ++ intWord r
          | x < 1000 = let (q,r) = (x `div` 100,x `rem` 100) in
                       if r==0 then intWord q ++ "hundred"
                       else intWord (q*100) ++ "and" ++ intWord r
          | otherwise = "onethousand"
