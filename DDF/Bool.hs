{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}
module DDF.Bool (module DDF.Bool, module DDF.DBI) where

import DDF.DBI
import qualified Prelude as M

class DBI r => Bool r where
  bool :: M.Bool -> r h M.Bool
  ite :: r h (a -> a -> M.Bool -> a)

ite3 = app3 ite