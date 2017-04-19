module DDF.Unit (module DDF.DBI, module DDF.Unit) where

import DDF.DBI

class DBI r => Unit r where
  unit :: r h ()
