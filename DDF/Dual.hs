{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction
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

dual1 = app dual
mkDual2 = app2 mkDual
dualOrig1 = app dualOrig
dualDiff1 = app dualDiff