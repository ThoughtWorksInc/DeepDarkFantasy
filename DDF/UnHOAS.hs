{-# LANGUAGE NoImplicitPrelude #-}

module DDF.UnHOAS where

import DDF.Lang

newtype UnHOAS repr h x = UnHOAS {runUnHOAS :: repr h x}

instance DBI repr => DBI (UnHOAS repr) where
  z = UnHOAS z
  s (UnHOAS x) = UnHOAS $ s x
  abs (UnHOAS x) = UnHOAS $ abs x
  app (UnHOAS f) (UnHOAS x) = UnHOAS $ app f x

instance Bool r => Bool (UnHOAS r) where
  bool = UnHOAS . bool
  ite = UnHOAS ite

instance Char r => Char (UnHOAS r) where
  char = UnHOAS . char

instance Lang repr => Lang (UnHOAS repr) where
  mkProd = UnHOAS mkProd
  zro = UnHOAS zro
  fst = UnHOAS fst
  double = UnHOAS . double
  doublePlus = UnHOAS doublePlus
  doubleMinus = UnHOAS doubleMinus
  doubleMult = UnHOAS doubleMult
  doubleDivide = UnHOAS doubleDivide
  doubleExp = UnHOAS doubleExp
  fix = UnHOAS fix
  left = UnHOAS left
  right = UnHOAS right
  sumMatch = UnHOAS sumMatch
  unit = UnHOAS unit
  exfalso = UnHOAS exfalso
  nothing = UnHOAS nothing
  just = UnHOAS just
  ioRet = UnHOAS ioRet
  ioBind = UnHOAS ioBind
  nil = UnHOAS nil
  cons = UnHOAS cons
  listMatch = UnHOAS listMatch
  optionMatch = UnHOAS optionMatch
  ioMap = UnHOAS ioMap
  writer = UnHOAS writer
  runWriter = UnHOAS runWriter
  float = UnHOAS . float
  floatPlus = UnHOAS floatPlus
  floatMinus = UnHOAS floatMinus
  floatMult = UnHOAS floatMult
  floatDivide = UnHOAS floatDivide
  floatExp = UnHOAS floatExp
  float2Double = UnHOAS float2Double
  double2Float = UnHOAS double2Float
  state = UnHOAS state
  runState = UnHOAS runState