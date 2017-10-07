module GradientDescent where

import Data.Matrix

type Mat = Matrix Double

generalGradientDescent :: (Mat -> Mat -> Mat -> Mat) -> (Mat -> Mat -> Mat -> Double) -> Mat -> Mat -> Mat -> Double -> Int -> (Mat,[Double])
generalGradientDescent funStep compCost x y theta alpha iters =
    let m = fromIntegral (nrows y) :: Double
        doGradientDescent currentIter (theta,hist) 
            | currentIter == 0 = (theta,reverse (compCost x y theta : hist))
            | otherwise = doGradientDescent (currentIter - 1) (funStep x y theta, compCost x y theta : hist)
    in doGradientDescent iters (theta,[])
