{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, UndecidableInstances #-}

module DDF.InfDiff where

import DDF.DLang
import qualified DDF.Map as Map
import DDF.Combine ()
import DDF.Diff ()

instance DBI r => DBI (InfDiff r v) where
  z = InfDiff z
  s (InfDiff x) = InfDiff $ s x
  abs (InfDiff x) = InfDiff $ abs x
  app (InfDiff f) (InfDiff x) = InfDiff $ app f x
  liftEnv (InfDiff x) = InfDiff $ liftEnv x

instance Dual r => Dual (InfDiff r v) where
  dual = InfDiff dual
  runDual = InfDiff runDual

instance Bimap r => Bimap (InfDiff r v) where

instance (Vector (InfDiff r v) v, Lang r) => Float (InfDiff r v) where
  float = InfDiff . float
  floatPlus = InfDiff floatPlus
  floatMinus = InfDiff floatMinus
  floatMult = InfDiff floatMult
  floatDivide = InfDiff floatDivide
  floatExp = InfDiff floatExp

instance (Vector (InfDiff r v) v, Dual r, Double r) => Double (InfDiff r v) where
  double = InfDiff . double
  doublePlus = InfDiff doublePlus
  doubleMinus = InfDiff doubleMinus
  doubleMult = InfDiff doubleMult
  doubleDivide = InfDiff doubleDivide
  doubleExp = InfDiff doubleExp

instance Char r => Char (InfDiff r v) where
  char = InfDiff . char

instance Bool r => Bool (InfDiff r v) where
  bool = InfDiff . bool
  ite = InfDiff ite

instance Prod r => Prod (InfDiff r v) where
  mkProd = InfDiff mkProd
  zro = InfDiff zro
  fst = InfDiff fst

instance Option r => Option (InfDiff r v) where
  nothing = InfDiff nothing
  just = InfDiff just
  optionMatch = InfDiff optionMatch

instance Map.Map r => Map.Map (InfDiff r v) where
  empty = InfDiff Map.empty
  singleton = InfDiff Map.singleton
  lookup = InfDiff Map.lookup
  alter = InfDiff Map.alter
  mapMap = InfDiff Map.mapMap

instance (Vector (InfDiff r v) v, Lang r) => Lang (InfDiff r v) where
  fix = InfDiff fix
  left = InfDiff left
  right = InfDiff right
  sumMatch = InfDiff sumMatch
  unit = InfDiff unit
  exfalso = InfDiff exfalso
  ioRet = InfDiff ioRet
  ioBind = InfDiff ioBind
  ioMap = InfDiff ioMap
  writer = InfDiff writer
  runWriter = InfDiff runWriter
  nil = InfDiff nil
  cons = InfDiff cons
  listMatch = InfDiff listMatch
  float2Double = InfDiff float2Double
  double2Float = InfDiff double2Float
  state = InfDiff state
  runState = InfDiff runState
  putStrLn = InfDiff putStrLn

instance (Vector (InfDiff r v) v, DLang r) => DLang (InfDiff r v) where
