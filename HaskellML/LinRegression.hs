module LinRegression (computeCost,gradientDescent,normalEqn) where

import Data.Matrix
import qualified Data.Vector as V
import qualified GradientDescent as GD
import qualified Debug.Trace as T


type Mat = Matrix Double

getScalar :: (Num a) => Matrix a -> a
getScalar m
    | nrows m /= 1 && ncols m /= 1 = error "this matrix cannot be seen as a scalar it is not 1x1 matrix"
    | otherwise = getElem 1 1 m


computeCost :: Mat -> Mat -> Mat -> Double
computeCost x y theta =
    let m = fromIntegral (nrows y) :: Double
        difference = (x * (transpose theta) - y)
        multSqr = 1.0 / (2.0 * m)
    in multSqr * getScalar (transpose difference * difference)

gradientDescent :: Mat -> Mat -> Mat -> Double -> Int -> (Mat,[Double])
gradientDescent x y theta alpha iters =
    let n = ncols x 
        funStep x y theta =
            let m = fromIntegral $ nrows y :: Double
                mult = 1.0 / m in
            let delta = transpose x * (x * (transpose theta) - y) in 
            theta - scaleMatrix (mult * alpha) (transpose delta)
    in GD.generalGradientDescent funStep computeCost x y theta alpha iters

normalEqn :: Mat -> Mat -> Mat
normalEqn x y =
    let xinv = inverse (transpose x * x) in
    case xinv of
        Left x -> error x
        Right m -> m * (transpose x) * y

