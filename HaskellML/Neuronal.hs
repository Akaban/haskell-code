module Neuronal where

import Data.Matrix

type Mat = Matrix Double

data NeuronNet = 
    NeuronNet { hiddenLayerNumber :: Int ,
      outputDimension :: Int ,
      inputDimension :: Int,
      weights :: [Mat] }

computeNeuronal :: Mat -> NeuronNet -> Mat
computeNeuronal input =
    (flip (*) input) . composeMatrixFromList . weights 
        where composeMatrixFromList = foldl1 (*) 

neuronalNetworkApp = flip computeNeuronal
