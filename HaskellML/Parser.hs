module Parser (parseData) where

import Data.Matrix
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

readDataUnsafe :: FilePath -> String
readDataUnsafe = unsafePerformIO . readFile

stringAsDouble :: String -> Double
stringAsDouble = read

parseString :: String -> Matrix Double
parseString = fromLists . map (\rowdata -> map stringAsDouble rowdata) .  map (wordsWhen (==',')) . drop 1 . lines

parseData :: FilePath -> (Matrix Double,Matrix Double)
parseData filePath = 
    let matrix = parseString . readDataUnsafe $ filePath
        (cols,rows) = (ncols matrix,nrows matrix)
    in (submatrix 1 rows 1 (cols-1) matrix,submatrix 1 rows cols cols matrix)
