{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies #-}

module DDF.RTInfDiff where

import DDF.Meta.Diff
import DDF.DLang
import DDF.Combine ()
import DDF.GDiff ()
import qualified DDF.Map as Map

instance DBI r => DBI (RTInfDiff r) where
  z = RTInfDiff z
  s (RTInfDiff x) = RTInfDiff (s x)
  abs (RTInfDiff x) = RTInfDiff (abs x)
  app (RTInfDiff f) (RTInfDiff x) = RTInfDiff (app f x)
  liftEnv (RTInfDiff x) = RTInfDiff (liftEnv x)

instance Prod r => Prod (RTInfDiff r) where
  zro = RTInfDiff zro
  fst = RTInfDiff fst
  mkProd = RTInfDiff mkProd

instance Dual r => Dual (RTInfDiff r) where
  dual = RTInfDiff dual
  runDual = RTInfDiff runDual

instance Option r => Option (RTInfDiff r) where
  nothing = RTInfDiff nothing
  just = RTInfDiff just
  optionMatch = RTInfDiff optionMatch

instance Bool r => Bool (RTInfDiff r) where
  bool x = RTInfDiff (bool x)
  ite = RTInfDiff ite

instance Char r => Char (RTInfDiff r) where
  char x = RTInfDiff (char x)

instance (Dual r, Double r) => Double (RTInfDiff r) where
  double x = RTInfDiff (double x)
  doublePlus = RTInfDiff doublePlus
  doubleMinus = RTInfDiff doubleMinus
  doubleMult = RTInfDiff doubleMult
  doubleDivide = RTInfDiff doubleDivide
  doubleExp = RTInfDiff doubleExp

instance Lang r => Float (RTInfDiff r) where
  float x = RTInfDiff (float x)
  floatPlus = RTInfDiff floatPlus
  floatMinus = RTInfDiff floatMinus
  floatMult = RTInfDiff floatMult
  floatDivide = RTInfDiff floatDivide
  floatExp = RTInfDiff floatExp

instance Map.Map r => Map.Map (RTInfDiff r) where
  empty = RTInfDiff Map.empty
  singleton = RTInfDiff Map.singleton
  alter = RTInfDiff Map.alter
  mapMap = RTInfDiff Map.mapMap
  lookup = RTInfDiff Map.lookup

instance Bimap r => Bimap (RTInfDiff r) where

instance Unit r => Unit (RTInfDiff r) where
  unit = RTInfDiff unit

instance Lang r => Lang (RTInfDiff r) where
  fix = RTInfDiff fix
  float2Double = RTInfDiff float2Double
  double2Float = RTInfDiff double2Float
  left = RTInfDiff left
  right = RTInfDiff right
  sumMatch = RTInfDiff sumMatch
  exfalso = RTInfDiff exfalso
  ioBind = RTInfDiff ioBind
  ioMap = RTInfDiff ioMap
  ioRet = RTInfDiff ioRet
  nil = RTInfDiff nil
  cons = RTInfDiff cons
  listMatch = RTInfDiff listMatch
  writer = RTInfDiff writer
  runWriter = RTInfDiff runWriter
  state = RTInfDiff state
  runState = RTInfDiff runState
  putStrLn = RTInfDiff putStrLn

instance DLang r => DLang (RTInfDiff r) where

diffInf :: (Vector (RTInfDiff r) v, DBI r) => Proxy v -> RTInfDiff r () x -> RTInfDiff r h (DiffType v x)
diffInf p (RTInfDiff (Combine _ (GDiff f))) = liftEnv $ runDiff $ f p