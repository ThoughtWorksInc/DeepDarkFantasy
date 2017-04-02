{-# LANGUAGE NoImplicitPrelude #-}

module DDF.Ordering (module DDF.Ordering, module DDF.DBI) where

import DDF.DBI
import qualified Prelude as M

class DBI r => Ordering r where
  ordering :: M.Ordering -> r h M.Ordering
  ltEqGt :: r h (a -> a -> a -> M.Ordering -> a)