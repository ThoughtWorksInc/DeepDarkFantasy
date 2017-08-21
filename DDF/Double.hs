{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleInstances,
  MultiParamTypeClasses,
  UndecidableInstances,
  UndecidableSuperClasses
#-}

module DDF.Double (module DDF.Double, module DDF.Ordering) where

import DDF.Ordering
import qualified Prelude as M

class (OrdC r M.Double, Ordering r) => Double r where
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
  doubleCmp :: r h (M.Double -> M.Double -> M.Ordering)

instance Double r => Ord r M.Double where
  cmp = doubleCmp
  nextOrd _ = Dict

doublePlus1 = app doublePlus
doublePlus2 = app2 doublePlus
doubleMinus2 = app2 doubleMinus
doubleMult2 = app2 doubleMult
doubleDivide2 = app2 doubleDivide
doubleExp1 = app doubleExp
