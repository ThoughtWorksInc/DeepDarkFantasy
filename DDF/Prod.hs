{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  ScopedTypeVariables
#-}

module DDF.Prod (module DDF.Prod, module DDF.Ordering) where

import DDF.Ordering

class Ordering r => Prod r where
  mkProd :: r h (a -> b -> (a, b))
  zro :: r h ((a, b) -> a)
  fst :: r h ((a, b) -> b)
  swap :: r h ((x, y) -> (y, x))
  swap = lam $ \p -> mkProd2 (fst1 p) (zro1 p)
  curry :: r h (((a, b) -> c) -> (a -> b -> c))
  curry = lam3 $ \f a b -> app f (mkProd2 a b)
  uncurry :: r h ((a -> b -> c) -> ((a, b) -> c))
  uncurry = lam2 $ \f p -> app2 f (zro1 p) (fst1 p)
  prodCmp :: forall h a b. r h (Cmp a -> Cmp b -> Cmp (a, b))
  prodCmp =
    lam2 $ \l r ->
      uncurry1 (lam2 $ \ll lr ->
        uncurry1 (lam2 $ \rl rr ->
          chainOrd2 (app2 l ll rl) (app2 r lr rr)))

zro1 = app1 zro
fst1 = app1 fst
mkProd1 = app1 mkProd
mkProd2 = app2 mkProd
curry1 = app curry
uncurry1 = app1 uncurry
