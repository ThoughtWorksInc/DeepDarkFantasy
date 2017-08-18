{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleContexts
#-}

module DDF.Int (module DDF.Int, module DDF.Ordering) where

import DDF.Ordering
import qualified Prelude as M

class (ObjOrd r M.Int, Ordering r) => Int r where
  int :: M.Int -> r h M.Int
  pred :: r h (M.Int -> M.Int)

pred1 = app pred