module DDF.DiffWrapper (module DDF.DiffWrapper, module DDF.DBI) where

import DDF.DBI
import DDF.Meta.DiffWrapper as DW

class DBI r => DiffWrapper r where
  diffWrapper :: r h (DW.FDiffType a x -> DW.DiffWrapper a x)
  runDiffWrapper :: r h (DW.DiffWrapper a x -> DW.FDiffType a x)