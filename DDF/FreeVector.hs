{-# LANGUAGE NoMonomorphismRestriction #-}

module DDF.FreeVector (module DDF.FreeVector, module DDF.DBI) where

import DDF.DBI
import qualified DDF.Meta.FreeVector as M

class DBI r => FreeVector r where
  freeVector :: r h ((b -> d) -> M.FreeVector b d)
  runFreeVector :: r h (M.FreeVector b d -> (b -> d))

freeVector1 = app freeVector
runFreeVector2 = app2 runFreeVector