{-# LANGUAGE NoMonomorphismRestriction #-}

module DDF.Sum (module DDF.Sum, module DDF.DBI) where

import DDF.DBI

class DBI r => Sum r where
  left :: r h (a -> Either a b)
  right :: r h (b -> Either a b)
  sumMatch :: r h ((a -> c) -> (b -> c) -> Either a b -> c)

sumMatch1 = app1 sumMatch
sumMatch2 = app2 sumMatch
sumMatch3 = app3 sumMatch
left1 = app left
right1 = app right