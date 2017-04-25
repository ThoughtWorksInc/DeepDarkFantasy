{-# LANGUAGE NoMonomorphismRestriction #-}

module DDF.Option (module DDF.Option, module DDF.DBI) where

import DDF.DBI

class DBI r => Option r where
  nothing :: r h (Maybe a)
  just :: r h (a -> Maybe a)
  optionMatch :: r h (b -> (a -> b) -> Maybe a -> b)

just1 = app just