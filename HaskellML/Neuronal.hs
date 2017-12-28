module Neuronal where

import Data.Matrix

type Mat = Matrix Double

data NeuronNet = 
    NeuronNet { hiddenLayerNumber :: Int ,
      outputDimension :: Int ,
      inputDimension :: Int,
      weights :: [Mat],
      activation :: Double -> Double }

-- Activation functions

sigmoid :: (Floating a) => a -> a
sigmoid t = 1 / (1 + exp (-t))

computeNeuronal :: Mat -> NeuronNet -> Mat
computeNeuronal input network =
    foldl (\computed weightInput
            -> activation' $
                weightInput * computed) input (weights network)
    where activation' = mapCol (\_ -> activation network) 1 

neuronalNetworkApp = flip computeNeuronal
