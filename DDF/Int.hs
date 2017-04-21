{-# LANGUAGE NoImplicitPrelude #-}

module DDF.Int (module DDF.Int, module DDF.DBI) where

import DDF.DBI
import qualified Prelude as M

class DBI r => Int r where
  int :: M.Int -> r h M.Int