{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, ScopedTypeVariables, TypeApplications, TypeFamilies #-}

module DDF.InfDiff where

import DDF.Meta.Diff
import DDF.Lang
import DDF.Combine ()
import DDF.GDiff ()
import qualified DDF.Map as Map

instance DBI r => DBI (InfDiff r) where
  z = InfDiff z
  s (InfDiff x) = InfDiff (s x)
  abs (InfDiff x) = InfDiff (abs x)
  app (InfDiff f) (InfDiff x) = InfDiff (app f x)
  liftEnv (InfDiff x) = InfDiff (liftEnv x)

instance Prod r => Prod (InfDiff r) where
  zro = InfDiff zro
  fst = InfDiff fst
  mkProd = InfDiff mkProd

instance Dual r => Dual (InfDiff r) where
  dual = InfDiff dual
  runDual = InfDiff runDual

instance Option r => Option (InfDiff r) where
  nothing = InfDiff nothing
  just = InfDiff just
  optionMatch = InfDiff optionMatch

instance Bool r => Bool (InfDiff r) where
  bool x = InfDiff (bool x)
  ite = InfDiff ite

instance Char r => Char (InfDiff r) where
  char x = InfDiff (char x)

instance (Dual r, Double r) => Double (InfDiff r) where
  double x = InfDiff (double x)
  doublePlus = InfDiff doublePlus
  doubleMinus = InfDiff doubleMinus
  doubleMult = InfDiff doubleMult
  doubleDivide = InfDiff doubleDivide
  doubleExp = InfDiff doubleExp

instance Lang r => Float (InfDiff r) where
  float x = InfDiff (float x)
  floatPlus = InfDiff floatPlus
  floatMinus = InfDiff floatMinus
  floatMult = InfDiff floatMult
  floatDivide = InfDiff floatDivide
  floatExp = InfDiff floatExp

instance Map.Map r => Map.Map (InfDiff r) where
  empty = InfDiff Map.empty
  singleton = InfDiff Map.singleton
  alter = InfDiff Map.alter
  mapMap = InfDiff Map.mapMap
  lookup = InfDiff Map.lookup

instance Bimap r => Bimap (InfDiff r) where

instance Unit r => Unit (InfDiff r) where
  unit = InfDiff unit

instance Sum r => Sum (InfDiff r) where
  left = InfDiff left
  right = InfDiff right
  sumMatch = InfDiff sumMatch

instance Int r => Int (InfDiff r) where
  int = InfDiff . int

instance Fix r => Fix (InfDiff r) where
  fix = InfDiff fix

instance List r => List (InfDiff r) where
  nil = InfDiff nil
  cons = InfDiff cons
  listMatch = InfDiff listMatch

instance IO r => IO (InfDiff r) where
  ioBind = InfDiff ioBind
  ioMap = InfDiff ioMap
  ioRet = InfDiff ioRet
  putStrLn = InfDiff putStrLn

instance Lang r => Lang (InfDiff r) where
  float2Double = InfDiff float2Double
  double2Float = InfDiff double2Float
  exfalso = InfDiff exfalso
  writer = InfDiff writer
  runWriter = InfDiff runWriter
  state = InfDiff state
  runState = InfDiff runState

diffInf :: (Vector (InfDiff r) v, DBI r) => Proxy v -> InfDiff r () x -> InfDiff r h (DiffType v x)
diffInf p (InfDiff (Combine _ (GDiff f))) = liftEnv $ runDiff $ f p