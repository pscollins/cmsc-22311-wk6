{-# LANGUAGE TemplateHaskell #-}

module ZipN where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH

mkExp' :: ExpQ -> [Name] -> ExpQ
mkExp' acc [] = acc
mkExp' acc (name:names) = mkExp' [| $(acc) <*> ZipList $(varE name) |] names

zipWithN :: Int -> ExpQ
zipWithN n = do
  names@(n:ns) <- mapM newName $ replicate n "x"
  fn <- newName "f"
  let vps = map varP (fn:names)
  lamE vps $ [| getZipList $(mkExp' [| $(varE fn) <$> ZipList $(varE n) |] ns) |]

-- zipWithN :: Int -> ExpQ
-- zipWithN n = [| $(zipN n) |]
