{-# LANGUAGE TemplateHaskell #-}

import ZipN
import Predicates


data Complex = Polar { r:: Double, angle :: Double }
             | Rectangular { re :: Double, im :: Double }

mkPredicates ''Complex

-- The typechecker confirms that our implementation is correct

zipWith1 :: (a -> b) -> [a] -> [b]
zipWith1 = $(zipWithN 1)

zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 = $(zipWithN 2)

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 = $(zipWithN 3)

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 = $(zipWithN 4)

main = do
  print $ $(zipWithN 1) id [1..10]
  print $ $(zipWithN 2) (+) [1..10] [2..30]


-- > ./Main
-- [1,2,3,4,5,6,7,8,9,10]
-- [3,5,7,9,11,13,15,17,19,21]
