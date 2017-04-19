{-# LANGUAGE NoImplicitPrelude #-}

module DDF.GInfDiff where

import DDF.DLang
import DDF.InfDiff ()
import qualified DDF.Map as Map

instance DBI r => DBI (GInfDiff r) where
  z = GInfDiff $ \_ -> z
  s (GInfDiff x) = GInfDiff $ s . x
  abs (GInfDiff x) = GInfDiff $ abs . x
  app (GInfDiff f) (GInfDiff x) = GInfDiff $ \p -> app (f p) (x p)
  liftEnv (GInfDiff x) = GInfDiff $ liftEnv . x

instance Dual r => Dual (GInfDiff r) where
  dual = GInfDiff $ \_ -> dual
  runDual = GInfDiff $ \_ -> runDual

instance Bimap r => Bimap (GInfDiff r) where

instance Lang r => Float (GInfDiff r) where
  float x = GInfDiff $ \_ -> float x
  floatPlus = GInfDiff $ \_ -> floatPlus
  floatMinus = GInfDiff $ \_ -> floatMinus
  floatMult = GInfDiff $ \_ -> floatMult
  floatDivide = GInfDiff $ \_ -> floatDivide
  floatExp = GInfDiff $ \_ -> floatExp

instance (Dual r, Double r) => Double (GInfDiff r) where
  double x = GInfDiff $ \_ -> double x
  doublePlus = GInfDiff $ \_ -> doublePlus
  doubleMinus = GInfDiff $ \_ -> doubleMinus
  doubleMult = GInfDiff $ \_ -> doubleMult
  doubleDivide = GInfDiff $ \_ -> doubleDivide
  doubleExp = GInfDiff $ \_ -> doubleExp

instance Char r => Char (GInfDiff r) where
  char x = GInfDiff $ \_ -> char x

instance Bool r => Bool (GInfDiff r) where
  bool x = GInfDiff $ \_ -> bool x
  ite = GInfDiff $ \_ -> ite

instance Prod r => Prod (GInfDiff r) where
  mkProd = GInfDiff $ \_ -> mkProd
  zro = GInfDiff $ \_ -> zro
  fst = GInfDiff $ \_ -> fst

instance Option r => Option (GInfDiff r) where
  nothing = GInfDiff $ \_ -> nothing
  just = GInfDiff $ \_ -> just
  optionMatch = GInfDiff $ \_ -> optionMatch

instance Map.Map r => Map.Map (GInfDiff r) where
  empty = GInfDiff $ \_ -> Map.empty
  singleton = GInfDiff $ \_ -> Map.singleton
  lookup = GInfDiff $ \_ -> Map.lookup
  alter = GInfDiff $ \_ -> Map.alter
  mapMap = GInfDiff $ \_ -> Map.mapMap

instance Lang r => Lang (GInfDiff r) where
  fix = GInfDiff $ \_ -> fix
  left = GInfDiff $ \_ -> left
  right = GInfDiff $ \_ -> right
  sumMatch = GInfDiff $ \_ -> sumMatch
  unit = GInfDiff $ \_ -> unit
  exfalso = GInfDiff $ \_ -> exfalso
  ioRet = GInfDiff $ \_ -> ioRet
  ioBind = GInfDiff $ \_ -> ioBind
  ioMap = GInfDiff $ \_ -> ioMap
  writer = GInfDiff $ \_ -> writer
  runWriter = GInfDiff $ \_ -> runWriter
  nil = GInfDiff $ \_ -> nil
  cons = GInfDiff $ \_ -> cons
  listMatch = GInfDiff $ \_ -> listMatch
  float2Double = GInfDiff $ \_ -> float2Double
  double2Float = GInfDiff $ \_ -> double2Float
  state = GInfDiff $ \_ -> state
  runState = GInfDiff $ \_ -> runState
  putStrLn = GInfDiff $ \_ -> putStrLn

instance DLang r => DLang (GInfDiff r) where
