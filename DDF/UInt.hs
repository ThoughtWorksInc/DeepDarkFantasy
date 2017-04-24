{-# LANGUAGE NoImplicitPrelude, TypeFamilies, MultiParamTypeClasses, PartialTypeSignatures, FlexibleInstances #-}

module DDF.UInt where

import DDF.Lang
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

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
  size = UInt
  empty = UInt
  singleton = UInt
  lookupL = UInt
  lookupR = UInt
  toMapL = UInt
  toMapR = UInt
  insert = UInt
  updateL = UInt
  updateR = UInt

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

instance Unit UInt where
  unit = UInt

instance Sum UInt where
  left = UInt
  right = UInt
  sumMatch = UInt

instance Int UInt where
  int _ = UInt

instance Fix UInt where
  fix = UInt

instance Functor UInt x where
  map = UInt

instance Applicative UInt x where
  ap = UInt
  pure = UInt

instance Monad UInt x where
  join = UInt
  bind = UInt

instance IO UInt where
  putStrLn = UInt

instance List UInt where
  nil = UInt
  cons = UInt
  listMatch = UInt

instance VTF.VectorTF UInt where
  zero = UInt
  basis = UInt
  plus = UInt
  mult = UInt
  vtfMatch = UInt

instance DiffWrapper UInt where
  diffWrapper = UInt
  runDiffWrapper = UInt

instance Lang UInt where
  exfalso = UInt
  runWriter = UInt
  writer = UInt
  double2Float = UInt
  float2Double = UInt
  state = UInt
  runState = UInt
