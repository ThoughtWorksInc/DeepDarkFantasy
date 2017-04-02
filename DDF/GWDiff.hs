
{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module DDF.GWDiff (module DDF.GWDiff, module DDF.Diff) where
import DDF.Lang
import qualified Prelude as M
import Data.Proxy
import DDF.Diff

newtype GWDiff r h x = GWDiff {runGWDiff :: forall v. Vector r v => Proxy v -> r (Diff v h) (Diff v x)}

instance DBI r => DBI (GWDiff r) where
  z = GWDiff (M.const z)
  s (GWDiff x) = GWDiff (\p -> s $ x p)
  app (GWDiff f) (GWDiff x) = GWDiff (\p -> app (f p) (x p))
  abs (GWDiff x) = GWDiff (\p -> abs $ x p)

instance Bool r => Bool (GWDiff r) where
  bool x = GWDiff $ M.const $ bool x
  ite = GWDiff $ M.const ite

instance Char r => Char (GWDiff r) where
  char x = GWDiff $ M.const $ char x

instance Prod r => Prod (GWDiff r) where
  mkProd = GWDiff (M.const mkProd)
  zro = GWDiff $ M.const zro
  fst = GWDiff $ M.const fst

instance (Double r, Prod r) => Double (GWDiff r) where
  double x = GWDiff $ M.const $ mkProd2 (double x) zero
  doublePlus = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  doubleMinus = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  doubleMult = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (mult2 (zro1 l) (zro1 r))
      (plus2 (mult2 (zro1 l) (fst1 r)) (mult2 (zro1 r) (fst1 l)))
  doubleDivide = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (zro1 r))
      (divide2 (minus2 (mult2 (zro1 r) (fst1 l)) (mult2 (zro1 l) (fst1 r)))
        (mult2 (zro1 r) (zro1 r)))
  doubleExp = GWDiff $ M.const $ lam $ \x -> mkProd2 (doubleExp1 (zro1 x)) (mult2 (doubleExp1 (zro1 x)) (fst1 x))

instance Lang r => Float (GWDiff r) where
  float x = GWDiff $ M.const $ mkProd2 (float x) zero
  floatPlus = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  floatMinus = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  floatMult = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (mult2 (float2Double1 (zro1 l)) (zro1 r))
      (plus2 (mult2 (float2Double1 (zro1 l)) (fst1 r)) (mult2 (float2Double1 (zro1 r)) (fst1 l)))
  floatDivide = GWDiff $ M.const $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (float2Double1 (zro1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (zro1 r)) (fst1 l)) (mult2 (float2Double1 (zro1 l)) (fst1 r)))
        (float2Double1 (mult2 (float2Double1 (zro1 r)) (zro1 r))))
  floatExp = GWDiff $ M.const $ lam $ \x -> mkProd2 (floatExp1 (zro1 x)) (mult2 (float2Double1 (floatExp1 (zro1 x))) (fst1 x))

instance Ordering r => Ordering (GWDiff r) where
  ordering x = GWDiff $ M.const $ ordering x
  ltEqGt = GWDiff $ M.const ltEqGt

instance Lang r => Lang (GWDiff r) where
  fix = GWDiff $ M.const fix
  left = GWDiff $ M.const left
  right = GWDiff $ M.const right
  sumMatch = GWDiff $ M.const sumMatch
  unit = GWDiff $ M.const unit
  exfalso = GWDiff $ M.const exfalso
  nothing = GWDiff $ M.const nothing
  just = GWDiff $ M.const just
  ioRet = GWDiff $ M.const ioRet
  ioBind = GWDiff $ M.const ioBind
  nil = GWDiff $ M.const nil
  cons = GWDiff $ M.const cons
  listMatch = GWDiff $ M.const listMatch
  optionMatch = GWDiff $ M.const optionMatch
  ioMap = GWDiff $ M.const ioMap
  writer = GWDiff $ M.const writer
  runWriter = GWDiff $ M.const runWriter
  float2Double = GWDiff $ M.const $ bimap2 float2Double id
  double2Float = GWDiff $ M.const $ bimap2 double2Float id
  state = GWDiff $ M.const state
  runState = GWDiff $ M.const runState
  putStrLn = GWDiff $ M.const putStrLn