{-# LANGUAGE NoImplicitPrelude #-}
module DDF.Char (module DDF.Char, module DDF.DBI) where

import DDF.DBI
import qualified Prelude as M

class DBI r => Char r where
  char :: M.Char -> r h M.Char
