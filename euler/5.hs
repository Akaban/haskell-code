import Data.List

assertDivisible n = all (\x -> n `mod` x == 0) [1..20]

result = take 1 [ x | x<-[1..], assertDivisible x]
