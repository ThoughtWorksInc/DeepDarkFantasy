{-# Language NoImplicitPrelude, NoMonomorphismRestriction #-}

module DDF.Ordering (module DDF.Ordering, module DDF.DBI) where

import DDF.DBI
import DDF.Bool
import qualified Prelude as M

class DBI repr => Ordering repr where
  sel :: repr h (a -> a -> a -> M.Ordering -> a)
  ordering :: M.Ordering -> repr h M.Ordering
  ltOrd :: repr h M.Ordering
  ltOrd = ordering M.LT
  eqOrd :: repr h M.Ordering
  eqOrd = ordering M.EQ
  gtOrd :: repr h M.Ordering
  gtOrd = ordering M.GT

sel1 = app1 sel
sel2 = app2 sel
sel3 = app3 sel
sel4 = app4 sel

isLT = sel3 true  false false
isEQ = sel3 false true  false
isGT = sel3 false false true