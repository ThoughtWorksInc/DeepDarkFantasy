{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction
#-}

module DDF.Double (module DDF.Double, module DDF.DBI) where

import DDF.DBI
import qualified Prelude as M

class DBI r => Double r where
  double :: M.Double -> r h M.Double
  doubleZero :: r h M.Double
  doubleZero = double 0
  doubleOne :: r h M.Double
  doubleOne = double 1
  doublePlus :: r h (M.Double -> M.Double -> M.Double)
  doubleMinus :: r h (M.Double -> M.Double -> M.Double)
  doubleMult :: r h (M.Double -> M.Double -> M.Double)
  doubleDivide :: r h (M.Double -> M.Double -> M.Double)
  doubleExp :: r h (M.Double -> M.Double)

doublePlus2 = app2 doublePlus
doubleMinus2 = app2 doubleMinus
doubleMult2 = app2 doubleMult
doubleDivide2 = app2 doubleDivide
doubleExp1 = app doubleExp
