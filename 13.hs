import System.IO.Unsafe (unsafePerformIO)

readF :: FilePath -> String
readF f = unsafePerformIO $ (readFile f)

splitNum :: String -> [String]
splitNum xs = go (concat (lines xs)) [] [] 1
   where go [] acc acc2 c = acc2
         go (x:xs) acc acc2 c
                     | c `mod` 50 == 0 = go xs [] (acc2 ++ [(acc ++ [x])]) 1
                     | otherwise = go xs (acc ++ [x]) acc2 (c+1)
