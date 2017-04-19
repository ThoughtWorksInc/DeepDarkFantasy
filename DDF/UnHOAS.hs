{-# LANGUAGE NoImplicitPrelude, TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module DDF.UnHOAS where

import DDF.DLang
import qualified DDF.Map as Map

instance DBI repr => DBI (UnHOAS repr) where
  z = UnHOAS z
  s (UnHOAS x) = UnHOAS $ s x
  abs (UnHOAS x) = UnHOAS $ abs x
  app (UnHOAS f) (UnHOAS x) = UnHOAS $ app f x
  liftEnv (UnHOAS x) = UnHOAS $ liftEnv x

instance Bool r => Bool (UnHOAS r) where
  bool = UnHOAS . bool
  ite = UnHOAS ite

instance Char r => Char (UnHOAS r) where
  char = UnHOAS . char

instance Prod r => Prod (UnHOAS r) where
  mkProd = UnHOAS mkProd
  zro = UnHOAS zro
  fst = UnHOAS fst

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

instance Option r => Option (UnHOAS r) where
  nothing = UnHOAS nothing
  just = UnHOAS just
  optionMatch = UnHOAS optionMatch

instance Map.Map r => Map.Map (UnHOAS r) where
  empty = UnHOAS Map.empty
  singleton = UnHOAS Map.singleton
  alter = UnHOAS Map.alter
  lookup = UnHOAS Map.lookup
  mapMap = UnHOAS Map.mapMap

instance Bimap r => Bimap (UnHOAS r) where

instance Dual r => Dual (UnHOAS r) where
  dual = UnHOAS dual
  runDual = UnHOAS runDual

instance DLang r => DLang (UnHOAS r) where
  float2Double = UnHOAS float2Double
  fix = UnHOAS fix
  left = UnHOAS left
  right = UnHOAS right
  sumMatch = UnHOAS sumMatch
  unit = UnHOAS unit
  exfalso = UnHOAS exfalso
  ioRet = UnHOAS ioRet
  ioBind = UnHOAS ioBind
  nil = UnHOAS nil
  cons = UnHOAS cons
  listMatch = UnHOAS listMatch
  ioMap = UnHOAS ioMap
  writer = UnHOAS writer
  runWriter = UnHOAS runWriter
  double2Float = UnHOAS double2Float
  state = UnHOAS state
  runState = UnHOAS runState
  putStrLn = UnHOAS putStrLn
  nextDiff p = UnHOAS (nextDiff p)
  infDiffGet = UnHOAS infDiffGet
  intDLang _ = intDLang @r Proxy
  infDiffApp = UnHOAS infDiffApp
  litInfDiff x = UnHOAS $ litInfDiff x
  rtDiffDiff _ p = rtDiffDiff @r Proxy p
  rtdd _ = rtdd @r Proxy

type instance RTDiff (UnHOAS r) x = RTDiff r x
type instance DiffInt (UnHOAS r) = DiffInt r
type instance DiffVector (UnHOAS r) v = DiffVector r v