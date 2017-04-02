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

instance Prod r => Prod (UnHOAS r) where
  mkProd = UnHOAS mkProd
  zro = UnHOAS zro
  fst = UnHOAS fst

instance Ordering r => Ordering (UnHOAS r) where
  ordering = UnHOAS . ordering
  ltEqGt = UnHOAS ltEqGt

instance Double r => Double (UnHOAS r) where
  double = UnHOAS . double
  doublePlus = UnHOAS doublePlus
  doubleMinus = UnHOAS doubleMinus
  doubleMult = UnHOAS doubleMult
  doubleDivide = UnHOAS doubleDivide
  doubleExp = UnHOAS doubleExp

instance Float r => Float (UnHOAS r) where
  float = UnHOAS . float
  floatPlus = UnHOAS floatPlus
  floatMinus = UnHOAS floatMinus
  floatMult = UnHOAS floatMult
  floatDivide = UnHOAS floatDivide
  floatExp = UnHOAS floatExp

instance Lang r => Lang (UnHOAS r) where
  float2Double = UnHOAS float2Double
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
  double2Float = UnHOAS double2Float
  state = UnHOAS state
  runState = UnHOAS runState
  putStrLn = UnHOAS putStrLn