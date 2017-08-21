{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleContexts,
  UndecidableSuperClasses,
  FlexibleInstances,
  MultiParamTypeClasses,
  UndecidableInstances
#-}

module DDF.Int (module DDF.Int, module DDF.Ordering) where

import DDF.Ordering
import qualified Prelude as M

class (OrdC r M.Int, Ordering r) => Int r where
  int :: M.Int -> r h M.Int
  pred :: r h (M.Int -> M.Int)
  intCmp :: r h (M.Int -> M.Int -> M.Ordering)

instance Int r => Ord r M.Int where
  cmp = intCmp
  nextOrd _ = Dict

pred1 = app pred
