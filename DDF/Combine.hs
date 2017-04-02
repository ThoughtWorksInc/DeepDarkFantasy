{-# LANGUAGE NoImplicitPrelude #-}

module DDF.Combine where

import DDF.Lang

data Combine l r h x = Combine (l h x) (r h x)

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

instance (Ordering l, Ordering r) => Ordering (Combine l r) where
  ordering x = Combine (ordering x) (ordering x)
  ltEqGt = Combine ltEqGt ltEqGt

instance (Lang l, Lang r) => Lang (Combine l r) where
  fix = Combine fix fix
  left = Combine left left
  right = Combine right right
  sumMatch = Combine sumMatch sumMatch
  unit = Combine unit unit
  exfalso = Combine exfalso exfalso
  nothing = Combine nothing nothing
  just = Combine just just
  optionMatch = Combine optionMatch optionMatch
  ioRet = Combine ioRet ioRet
  ioBind = Combine ioBind ioBind
  ioMap = Combine ioMap ioMap
  nil = Combine nil nil
  cons = Combine cons cons
  listMatch = Combine listMatch listMatch
  runWriter = Combine runWriter runWriter
  writer = Combine writer writer
  double2Float = Combine double2Float double2Float
  float2Double = Combine float2Double float2Double
  state = Combine state state
  runState = Combine runState runState
  putStrLn = Combine putStrLn putStrLn