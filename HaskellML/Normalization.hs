module Normalization where

import Numeric.Statistics
import qualified Data.Vector as V
import Data.Matrix

type Mat = Matrix Double

scaleMatrixFeature :: Mat -> (Mat,Mat,Mat)
scaleMatrixFeature xMat =
    let n = ncols xMat
        mu = matrix n 1 (\(i,_) -> mean . V.toList  $ getCol i xMat )
        sigma = matrix n 1 (\(i,_) -> stddev . V.toList $ getCol i xMat)
    in (foldl (\mat col -> mapCol (\_ x -> (x - getElem col 1 mu) / getElem col 1 sigma) col mat) xMat [1..n],mu,sigma)

scaleVector :: Mat -> Mat -> Mat -> Mat
scaleVector mu sigma v =
    let rows = nrows v
        matrix = submatrix 2 rows 1 1 v
        el = fromLists . return . return $ getElem 1 1 v
        fstMatrix = elementwise (-) matrix mu
    in el <-> elementwise (/) fstMatrix sigma
