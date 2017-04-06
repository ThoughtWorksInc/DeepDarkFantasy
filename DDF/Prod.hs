{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

module DDF.Prod (module DDF.Prod, module DDF.DBI) where

import DDF.DBI

class DBI r => Prod r where
  mkProd :: r h (a -> b -> (a, b))
  zro :: r h ((a, b) -> a)
  fst :: r h ((a, b) -> b)
  swap :: r h ((x, y) -> (y, x))
  swap = lam $ \p -> mkProd2 (fst1 p) (zro1 p)
  curry :: r h (((a, b) -> c) -> (a -> b -> c))
  curry = lam3 $ \f a b -> app f (mkProd2 a b)
  uncurry :: r h ((a -> b -> c) -> ((a, b) -> c))
  uncurry = lam2 $ \f p -> app2 f (zro1 p) (fst1 p)

zro1 = app zro
fst1 = app fst
mkProd1 = app mkProd
mkProd2 = app2 mkProd
curry1 = app curry