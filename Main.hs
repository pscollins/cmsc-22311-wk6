{-# LANGUAGE TemplateHaskell #-}

import ZipN

main = do
  print $ $(zip2) [1..10] [2..20]
  print $ $(zipWith2) (+) [1..10] [10..20]
  print $ $(zipWith2') (+) [1..10] [10..20]
