{-# LANGUAGE TemplateHaskell #-}

module ZipN where

import Control.Applicative

import Language.Haskell.TH


zip2 :: ExpQ
zip2 = do
  a <- newName "a"
  b <- newName "b"
  [| \ $(varP a) -> \ $(varP b) -> zip $(varE a) $(varE b) |]

zip2' :: ExpQ
zip2' = do
  names <- repeat 2 mkName
  lamE (map varP names) <*> 'zipWith

zipWith2 :: ExpQ
zipWith2 = do
  a <- newName "a"
  b <- newName "b"
  f <- newName "f"
  [| \ $(varP f) -> \ $(varP a) -> \ $(varP b) -> zipWith $(varE f) $(varE a) $(varE b) |]


zipWith2' :: ExpQ
zipWith2' = do
  a <- newName "a"
  b <- newName "b"
  f <- newName "f"
  [| \ $(varP f) -> \ $(varP a) -> \ $(varP b) -> getZipList $ $(varE f) <$> ZipList $(varE a) <*> ZipList $(varE b) |]


-- zipWithN :: Int -> ExpQ
-- zipWithN n = do
--   f <- newName "f"
--   vs <- replicate n mkName
--   mkLam $ map varP f:vs $ [
--   fnBodies <- map
--   [|

-- zipWithN :: Int -> ExpQ
-- zipWithN n
--    | n <= 0 = fail "Invalid zipWithN"
--    | otherwise = newName "f" >>= \f ->
--                  getZipList $ [| \ $(VarP f) -> $(VarE f) <$> $(loop n) |]
--    where loop n = mkName >>= \a -> [| \ $(VarP a) -> $(loop $ n - 1) |]
--          loop 1 = mkName >>= \a -> [| \ $(VarP a) -> ZipList a |]
