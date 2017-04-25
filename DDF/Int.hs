{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

module DDF.Int (module DDF.Int, module DDF.Bool) where

import DDF.Bool
import qualified Prelude as M

class Bool r => Int r where
  int :: M.Int -> r h M.Int
  pred :: r h (M.Int -> M.Int)
  isZero :: r h (M.Int -> M.Bool)

pred1 = app pred
isZero1 = app isZero