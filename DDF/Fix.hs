{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

module DDF.Fix (module DDF.Fix, module DDF.DBI) where

import DDF.DBI

class DBI r => Fix r where
  fix :: r h ((a -> a) -> a)
  undefined :: r h a
  undefined = fix1 id

fix1 = app fix
fix2 = app2 fix
