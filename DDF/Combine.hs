{-# LANGUAGE
  NoImplicitPrelude,
  TypeFamilies,
  TypeApplications,
  ScopedTypeVariables,
  NoMonomorphismRestriction,
  MultiParamTypeClasses,
  FlexibleInstances,
  FlexibleContexts
#-}

module DDF.Combine where

import DDF.Meta.Interpreter
import qualified DDF.Map as Map
import DDF.Lang
import qualified DDF.VectorTF as VTF

instance (DBI l, DBI r) => DBI (Combine l r) where
  z = Combine z z
  s (Combine l r) = Combine (s l) (s r)
  app (Combine fl fr) (Combine xl xr) = Combine (app fl xl) (app fr xr)
  abs (Combine l r) = Combine (abs l) (abs r)
  hoas f = Combine (hoas $ \x -> case f (Combine x z) of Combine l _ -> l) (hoas $ \x -> case f (Combine z x) of Combine _ r -> r)

instance (Bool l, Bool r) => Bool (Combine l r) where
  bool x = Combine (bool x) (bool x)
  ite = Combine ite ite

instance (Char l, Char r) => Char (Combine l r) where
  char x = Combine (char x) (char x)

instance (Prod l, Prod r) => Prod (Combine l r) where
  mkProd = Combine mkProd mkProd
  zro = Combine zro zro
  fst = Combine fst fst

instance (Double l, Double r) => Double (Combine l r) where
  double x = Combine (double x) (double x)
  doublePlus = Combine doublePlus doublePlus
  doubleMinus = Combine doubleMinus doubleMinus
  doubleMult = Combine doubleMult doubleMult
  doubleDivide = Combine doubleDivide doubleDivide
  doubleExp = Combine doubleExp doubleExp

instance (Float l, Float r) => Float (Combine l r) where
  float x = Combine (float x) (float x)
  floatPlus = Combine floatPlus floatPlus
  floatMinus = Combine floatMinus floatMinus
  floatMult = Combine floatMult floatMult
  floatDivide = Combine floatDivide floatDivide
  floatExp = Combine floatExp floatExp

instance (Option l, Option r) => Option (Combine l r) where
  nothing = Combine nothing nothing
  just = Combine just just
  optionMatch = Combine optionMatch optionMatch

instance (Map.Map l, Map.Map r) => Map.Map (Combine l r) where
  empty = Combine Map.empty Map.empty
  lookup = Combine Map.lookup Map.lookup
  singleton = Combine Map.singleton Map.singleton
  alter = Combine Map.alter Map.alter
  mapMap = Combine Map.mapMap Map.mapMap
  unionWith = Combine Map.unionWith Map.unionWith

instance (Bimap l, Bimap r) => Bimap (Combine l r) where
  size = Combine size size
  empty = Combine empty empty
  singleton = Combine singleton singleton
  lookupL = Combine lookupL lookupL
  lookupR = Combine lookupR lookupR
  toMapL = Combine toMapL toMapL
  toMapR = Combine toMapR toMapR
  insert = Combine insert insert
  updateL = Combine updateL updateL
  updateR = Combine updateR updateR

instance (Dual l, Dual r) => Dual (Combine l r) where
  dual = Combine dual dual
  runDual = Combine runDual runDual

instance (Unit l, Unit r) => Unit (Combine l r) where
  unit = Combine unit unit

instance (Sum l, Sum r) => Sum (Combine l r) where
  left = Combine left left
  right = Combine right right
  sumMatch = Combine sumMatch sumMatch

instance (Int l, Int r) => Int (Combine l r) where
  int x = Combine (int x) (int x)
  pred = Combine pred pred
  isZero = Combine isZero isZero

instance (Y l, Y r) => Y (Combine l r) where
  y = Combine y y

instance (List l, List r) => List (Combine l r) where
  nil = Combine nil nil
  cons = Combine cons cons
  listMatch = Combine listMatch listMatch

instance (Functor l m, Functor r m) => Functor (Combine l r) m where
  map = Combine map map

instance (Applicative l m, Applicative r m) => Applicative (Combine l r) m where
  pure = Combine pure pure
  ap = Combine ap ap

instance (Monad l m, Monad r m) => Monad (Combine l r) m where
  bind = Combine bind bind
  join = Combine join join

instance (IO l, IO r) => IO (Combine l r) where
  putStrLn = Combine putStrLn putStrLn

instance (VTF.VectorTF l, VTF.VectorTF r) => VTF.VectorTF (Combine l r) where
  zero = Combine VTF.zero VTF.zero
  basis = Combine VTF.basis VTF.basis
  plus = Combine VTF.plus VTF.plus
  mult = Combine VTF.mult VTF.mult
  vtfMatch = Combine VTF.vtfMatch VTF.vtfMatch

instance (DiffWrapper l, DiffWrapper r) => DiffWrapper (Combine l r) where
  diffWrapper = Combine diffWrapper diffWrapper
  runDiffWrapper = Combine runDiffWrapper runDiffWrapper

instance (Fix l, Fix r) => Fix (Combine l r) where
  fix = Combine fix fix
  runFix = Combine runFix runFix

instance (FreeVector l, FreeVector r) => FreeVector (Combine l r) where
  freeVector = Combine freeVector freeVector
  runFreeVector = Combine runFreeVector runFreeVector

instance (Lang l, Lang r) => Lang (Combine l r) where
  exfalso = Combine exfalso exfalso
  runWriter = Combine runWriter runWriter
  writer = Combine writer writer
  double2Float = Combine double2Float double2Float
  float2Double = Combine float2Double float2Double
  state = Combine state state
  runState = Combine runState runState
