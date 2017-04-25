{-# LANGUAGE NoMonomorphismRestriction, NoImplicitPrelude #-}

module DDF.Fix (module DDF.Fix, module DDF.DBI) where

import DDF.DBI
import qualified Data.Functor.Foldable as M

class DBI r => Fix r where
  fix :: r h (f (M.Fix f) -> M.Fix f)
  runFix :: r h (M.Fix f -> f (M.Fix f))

fix1 = app fix
runFix1 = app runFix