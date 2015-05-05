{-# LANGUAGE TemplateHaskell #-}

import ZipN

main = do
  let vs = $(zipN 1) id [1..10]
  let vs' = $(zipN 2) id [1..10] [2..30]
  let vs'' = $(zipN 3) id [1..10] [2..30] [4..50]
  return ()
