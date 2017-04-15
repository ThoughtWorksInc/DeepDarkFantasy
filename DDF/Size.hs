{-# LANGUAGE NoImplicitPrelude #-}

module DDF.Size where

import DDF.Lang
import qualified DDF.Map as Map

newtype Size h x = Size {runSize :: Int}

one = Size 1

instance DBI Size where
  z = one
  s (Size x) = (Size x)
  app (Size l) (Size r) = Size (l + r)
  abs (Size l) = Size (1 + l)

instance Bool Size where
  bool _ = one
  ite = one

instance Char Size where
  char _ = one

instance Option Size where
  nothing = one
  just = one
  optionMatch = one

instance Double Size where
  double _ = one
  doublePlus = one
  doubleMinus = one
  doubleMult = one
  doubleDivide = one
  doubleExp = one

instance Float Size where
  float _ = one
  floatPlus = one
  floatMinus = one
  floatMult = one
  floatDivide = one
  floatExp = one

instance Map.Map Size where
  mapMap = one
  alter = one
  empty = one
  singleton = one
  lookup = one

instance Prod Size where
  mkProd = one
  zro = one
  fst = one

instance Dual Size where
  dual = one
  runDual = one

instance Bimap Size where

instance Lang Size where
  fix = one
  left = one
  right = one
  sumMatch = one
  unit = one
  exfalso = one
  ioRet = one
  ioBind = one
  ioMap = one
  nil = one
  cons = one
  listMatch = one
  writer = one
  runWriter = one
  float2Double = one
  double2Float = one
  state = one
  runState = one
  putStrLn = one
  nextDiff _ = one