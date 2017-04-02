{-# LANGUAGE NoImplicitPrelude #-}

module DDF.WDiff where

import DDF.Lang
import DDF.Diff

newtype WDiff r v h x = WDiff {runWDiff :: r (Diff v h) (Diff v x)}

instance DBI r => DBI (WDiff r v) where
  z = WDiff z
  s (WDiff x) = WDiff $ s x
  abs (WDiff f) = WDiff $ abs f
  app (WDiff f) (WDiff x) = WDiff $ app f x
  hoas f = WDiff $ hoas (runWDiff . f . WDiff)

instance Bool r => Bool (WDiff r v) where
  bool = WDiff . bool
  ite = WDiff ite

instance Char r => Char (WDiff r v) where
  char = WDiff . char

instance (Vector r v, Lang r) => Lang (WDiff r v) where
  mkProd = WDiff mkProd
  zro = WDiff zro
  fst = WDiff fst
  double x = WDiff $ mkProd2 (double x) zero
  doublePlus = WDiff $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  doubleMinus = WDiff $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  doubleMult = WDiff $ lam2 $ \l r ->
    mkProd2 (mult2 (zro1 l) (zro1 r))
      (plus2 (mult2 (zro1 l) (fst1 r)) (mult2 (zro1 r) (fst1 l)))
  doubleDivide = WDiff $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (zro1 r))
      (divide2 (minus2 (mult2 (zro1 r) (fst1 l)) (mult2 (zro1 l) (fst1 r)))
        (mult2 (zro1 r) (zro1 r)))
  doubleExp = WDiff $ lam $ \x -> mkProd2 (doubleExp1 (zro1 x)) (mult2 (doubleExp1 (zro1 x)) (fst1 x))
  fix = WDiff fix
  left = WDiff left
  right = WDiff right
  sumMatch = WDiff sumMatch
  unit = WDiff unit
  exfalso = WDiff exfalso
  nothing = WDiff nothing
  just = WDiff just
  ioRet = WDiff ioRet
  ioBind = WDiff ioBind
  nil = WDiff nil
  cons = WDiff cons
  listMatch = WDiff listMatch
  optionMatch = WDiff optionMatch
  ioMap = WDiff ioMap
  writer = WDiff writer
  runWriter = WDiff runWriter
  float x = WDiff $ mkProd2 (float x) zero
  floatPlus = WDiff $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  floatMinus = WDiff $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  floatMult = WDiff $ lam2 $ \l r ->
    mkProd2 (mult2 (float2Double1 (zro1 l)) (zro1 r))
      (plus2 (mult2 (float2Double1 (zro1 l)) (fst1 r)) (mult2 (float2Double1 (zro1 r)) (fst1 l)))
  floatDivide = WDiff $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (float2Double1 (zro1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (zro1 r)) (fst1 l)) (mult2 (float2Double1 (zro1 l)) (fst1 r)))
        (float2Double1 (mult2 (float2Double1 (zro1 r)) (zro1 r))))
  floatExp = WDiff $ lam $ \x -> mkProd2 (floatExp1 (zro1 x)) (mult2 (float2Double1 (floatExp1 (zro1 x))) (fst1 x))
  float2Double = WDiff $ bimap2 float2Double id
  double2Float = WDiff $ bimap2 double2Float id
  state = WDiff state
  runState = WDiff runState
