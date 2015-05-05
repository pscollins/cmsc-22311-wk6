{-# LANGUAGE TemplateHaskell #-}

module ZipN where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH


mkExp :: [Name] -> ExpQ
mkExp (name:[]) = [| ZipList $(varE name) |]
mkExp (name:names) = [| ZipList $(varE name) <*> $(mkExp names) |]

zipN :: Int -> ExpQ
zipN n = do
  names <- mapM newName $ replicate n "x"
  fn <- newName "f"
  let vps = map varP (fn:names)
  lamE vps $ [| $(varE fn) <$> $(mkExp names) |]
