{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module DDF.Size where

import DDF.Lang
import qualified Prelude as M
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

type instance OrdC Size = NoOrdC

newtype Size h x = Size {runSize :: M.Int}

one = Size 1

instance DBI Size where
  z = one
  s (Size x) = Size x
  app (Size l) (Size r) = Size (l + r)
  abs (Size x) = Size x

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
  doubleCmp = one

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
  unionWith = one

instance Prod Size where
  mkProd = one
  zro = one
  fst = one

instance Dual Size where
  dual = one
  runDual = one
  dualNextOrd = Sub Dict

instance Bimap Size where
  updateL = one
  updateR = one
  singleton = one
  empty = one
  insert = one
  lookupL = one
  lookupR = one
  size = one
  toMapL = one
  toMapR = one

instance Unit Size where
  unit = one

instance Sum Size where
  left = one
  right = one
  sumMatch = one

instance Int Size where
  int _ = one
  pred = one
  intCmp = one

instance IO Size where
  putStrLn = one
  ioJoin = one
  ioBind = one
  ioMap = one
  ioPure = one
  ioAP = one

instance Y Size where
  y = one

instance List Size where
  nil = one
  cons = one
  listMatch = one

instance VTF.VectorTF Size where
  zero = one
  basis = one
  plus = one
  mult = one
  vtfMatch = one
  vtfCmp = one
  vtfNextOrd = Sub Dict

instance DiffWrapper Size where
  diffWrapper = one
  runDiffWrapper = one

instance Fix Size where
  fix = one
  runFix = one

instance FreeVector Size where
  freeVector = one
  runFreeVector = one

instance Lang Size where
  exfalso = one
  writer = one
  runWriter = one
  float2Double = one
  double2Float = one
  state = one
  runState = one

instance Ordering Size where
  sel = one
  ordering _ = one
