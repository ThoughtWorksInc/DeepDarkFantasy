{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

module DDF.Y (module DDF.Y, module DDF.DBI) where

import DDF.DBI

class DBI r => Y r where
  y :: r h ((a -> a) -> a)
  undefined :: r h a
  undefined = y1 id

y1 = app y
y2 = app2 y
