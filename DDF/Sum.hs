{-# LANGUAGE NoMonomorphismRestriction #-}

module DDF.Sum (module DDF.Sum, module DDF.DBI) where

import DDF.DBI

class DBI r => Sum r where
  left :: r h (a -> Either a b)
  right :: r h (b -> Either a b)
  sumMatch :: r h ((a -> c) -> (b -> c) -> Either a b -> c)

sumMatch2 = app2 sumMatch
