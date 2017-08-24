{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeOperators,
  TypeApplications,
  ScopedTypeVariables
#-}

module DDF.Dual (module DDF.Dual, module DDF.Prod) where

import DDF.Prod
import qualified DDF.Meta.Dual as M

class Prod r => Dual r where
  dual :: r h ((x, y) -> M.Dual x y)
  runDual :: r h (M.Dual x y -> (x, y))
  mkDual :: r h (x -> y -> M.Dual x y)
  mkDual = curry1 dual
  dualOrig :: r h (M.Dual x y -> x)
  dualOrig = zro `com2` runDual
  dualDiff :: r h (M.Dual x y -> y)
  dualDiff = fst `com2` runDual
  dualCmp :: r h (Cmp x -> Cmp (M.Dual x y))
  dualCmp = cmpMap1 dualOrig
  dualGetOrdC :: Ord r x :- OrdC r (M.Dual x y)

dual1 = app dual
mkDual2 = app2 mkDual
dualOrig1 = app dualOrig
dualDiff1 = app dualDiff
runDual1 = app1 runDual

instance (Ord r x, Dual r) => Ord r (M.Dual x y) where
  cmp = app dualCmp cmp
  getOrdC _ = Dict \\ dualGetOrdC @r @x @y
