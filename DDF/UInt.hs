{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

module DDF.UInt where

import DDF.DLang
import qualified DDF.Map as Map

data UInt h x = UInt

instance DBI UInt where
  z = UInt
  s _ = UInt
  abs _ = UInt
  app _ _ = UInt
  liftEnv _ = UInt

instance Bool UInt where
  bool _ = UInt
  ite = UInt

instance Char UInt where
  char _ = UInt

instance Double UInt where
  double _ = UInt
  doublePlus = UInt
  doubleMinus = UInt
  doubleMult = UInt
  doubleDivide = UInt
  doubleExp = UInt

instance Float UInt where
  float _ = UInt
  floatPlus = UInt
  floatMinus = UInt
  floatMult = UInt
  floatDivide = UInt
  floatExp = UInt

instance Bimap UInt where

instance Dual UInt where
  dual = UInt
  runDual = UInt

instance Map.Map UInt where
  empty = UInt
  singleton = UInt
  lookup = UInt
  alter = UInt
  mapMap = UInt

instance Prod UInt where
  mkProd = UInt
  zro = UInt
  fst = UInt

instance Option UInt where
  nothing = UInt
  just = UInt
  optionMatch = UInt

instance Lang UInt where
  fix = UInt
  left = UInt
  right = UInt
  sumMatch = UInt
  unit = UInt
  exfalso = UInt
  ioRet = UInt
  ioBind = UInt
  ioMap = UInt
  nil = UInt
  cons = UInt
  listMatch = UInt
  runWriter = UInt
  writer = UInt
  double2Float = UInt
  float2Double = UInt
  state = UInt
  runState = UInt
  putStrLn = UInt

instance DLang UInt where
