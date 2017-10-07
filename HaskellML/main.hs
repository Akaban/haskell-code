module Main where

import qualified LinRegression as R
import qualified Data.Vector as V
import qualified Parser as Parser
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import Data.Matrix
import Normalization
import Data.Char (toUpper)

one :: Num a =>
     Int 
  -> Int 
  -> Matrix a
one n m = matrix n m (const 1)


colVectorAsFloat :: (Integral a) => [a] -> Matrix Double
colVectorAsFloat = fmap (fromIntegral) . colVector . V.fromList

addOneCol :: Matrix Double -> Matrix Double
addOneCol m =
    let rows = nrows m in
    (one rows 1) <|> m 

roundFloat :: Int -> Double -> Double
roundFloat n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)


sizeMatrix m = show (nrows m) ++ "x" ++ show (ncols m)

capitalize :: String -> String
capitalize (x:xs) =
    toUpper x : xs

main = do
    arg <- getArgs
    if length arg /= 7 then  putStrLn "Arguments missing." else do
    let (x,y) = Parser.parseData (arg !! 0)

    let (xNormalized,mu,sigma) = scaleMatrixFeature x -- Normalize the matrix to scale features
        matXNormalized = addOneCol xNormalized
        matX = addOneCol x

    let useGradientDescent = read (capitalize $ arg !! 3) :: Bool

    let alpha = read (arg !! 1) :: Double --learning rate
        iterations = read (arg !! 2) :: Int -- iterations
        (theta,jhistory) = if useGradientDescent then R.gradientDescent matXNormalized y (zero 1 (ncols matXNormalized)) alpha iterations
                           else (transpose $ R.normalEqn matX y,[])

    let predictionList = map (read)  $ drop 4 arg :: [Double]
        predictionVector = scaleVector mu sigma . colVector . V.fromList $ predictionList
        prediction = theta * predictionVector
    putStrLn $ "The prediction is: " ++ show (getElem 1 1 prediction) 

