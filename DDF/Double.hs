{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction
#-}

module DDF.Double (module DDF.Double, module DDF.Bool) where

import DDF.Bool
import qualified Prelude as M

class Bool r => Double r where
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
  doubleEq :: r h (M.Double -> M.Double -> M.Bool)

doublePlus1 = app doublePlus
doublePlus2 = app2 doublePlus
doubleMinus2 = app2 doubleMinus
doubleMult2 = app2 doubleMult
doubleDivide2 = app2 doubleDivide
doubleExp1 = app doubleExp
doubleEq2 = app2 doubleEq
