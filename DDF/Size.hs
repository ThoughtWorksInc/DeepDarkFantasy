{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

module DDF.Size where

import DDF.Lang
import qualified Prelude as M
import qualified DDF.Map as Map

newtype Size h x = Size {runSize :: M.Int}

one = Size 1

instance DBI Size where
  z = one
  s (Size x) = (Size x)
  app (Size l) (Size r) = Size (l + r)
  abs (Size l) = Size (1 + l)
  liftEnv (Size l) = Size l

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

instance Unit Size where
  unit = one

instance Sum Size where
  left = one
  right = one
  sumMatch = one

instance Int Size where
  int _ = one

instance Lang Size where
  fix = one
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
