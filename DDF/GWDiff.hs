{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  TypeFamilies,
  TypeApplications
#-}

module DDF.GWDiff (module DDF.Diff) where
import DDF.DLang
import qualified Prelude as M
import DDF.Diff
import qualified Data.Map as M
import qualified DDF.Map as Map

instance DBI r => DBI (GWDiff r) where
  z = GWDiff (M.const z)
  s (GWDiff x) = GWDiff (\p -> s $ x p)
  app (GWDiff f) (GWDiff x) = GWDiff (\p -> app (f p) (x p))
  abs (GWDiff x) = GWDiff (\p -> abs $ x p)
  liftEnv (GWDiff x) = GWDiff (\p -> liftEnv $ x p)

instance Bool r => Bool (GWDiff r) where
  bool x = GWDiff $ M.const $ bool x
  ite = GWDiff $ M.const ite

instance Char r => Char (GWDiff r) where
  char x = GWDiff $ M.const $ char x

instance Prod r => Prod (GWDiff r) where
  mkProd = GWDiff (M.const mkProd)
  zro = GWDiff $ M.const zro
  fst = GWDiff $ M.const fst

instance Dual r => Dual (GWDiff r) where
  dual = GWDiff $ M.const $ dual
  runDual = GWDiff $ M.const $ runDual

instance (Double r, Dual r) => Double (GWDiff r) where
  double x = GWDiff $ M.const $ mkDual2 (double x) zero
  doublePlus = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (plus2 (dualOrig1 l) (dualOrig1 r)) (plus2 (dualDiff1 l) (dualDiff1 r))
  doubleMinus = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (minus2 (dualOrig1 l) (dualOrig1 r)) (minus2 (dualDiff1 l) (dualDiff1 r))
  doubleMult = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (mult2 (dualOrig1 l) (dualOrig1 r))
      (plus2 (mult2 (dualOrig1 l) (dualDiff1 r)) (mult2 (dualOrig1 r) (dualDiff1 l)))
  doubleDivide = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (divide2 (dualOrig1 l) (dualOrig1 r))
      (divide2 (minus2 (mult2 (dualOrig1 r) (dualDiff1 l)) (mult2 (dualOrig1 l) (dualDiff1 r)))
        (mult2 (dualOrig1 r) (dualOrig1 r)))
  doubleExp = GWDiff $ M.const $ lam $ \x -> let_2 (doubleExp1 (dualOrig1 x)) $ lam $ \e -> mkDual2 e (mult2 e (dualDiff1 x))

instance Lang r => Float (GWDiff r) where
  float x = GWDiff $ M.const $ mkDual2 (float x) zero
  floatPlus = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (plus2 (dualOrig1 l) (dualOrig1 r)) (plus2 (dualDiff1 l) (dualDiff1 r))
  floatMinus = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (minus2 (dualOrig1 l) (dualOrig1 r)) (minus2 (dualDiff1 l) (dualDiff1 r))
  floatMult = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (mult2 (float2Double1 (dualOrig1 l)) (dualOrig1 r))
      (plus2 (mult2 (float2Double1 (dualOrig1 l)) (dualDiff1 r)) (mult2 (float2Double1 (dualOrig1 r)) (dualDiff1 l)))
  floatDivide = GWDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (divide2 (dualOrig1 l) (float2Double1 (dualOrig1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (dualOrig1 r)) (dualDiff1 l)) (mult2 (float2Double1 (dualOrig1 l)) (dualDiff1 r)))
        (float2Double1 (mult2 (float2Double1 (dualOrig1 r)) (dualOrig1 r))))
  floatExp = GWDiff $ M.const $ lam $ \x -> let_2 (floatExp1 (dualOrig1 x)) $ lam $ \e -> mkDual2 e (mult2 (float2Double1 e) (dualDiff1 x))

instance Option r => Option (GWDiff r) where
  nothing = GWDiff $ M.const nothing
  just = GWDiff $ M.const just
  optionMatch = GWDiff $ M.const optionMatch

instance Map.Map r => Map.Map (GWDiff r) where
  empty = GWDiff $ M.const Map.empty
  singleton = GWDiff $ M.const Map.singleton
  lookup :: forall h k a. Map.Ord k => GWDiff r h (k -> M.Map k a -> Maybe a)
  lookup = GWDiff $ \(_ :: Proxy v) -> withDict (Map.diffOrd (Proxy :: Proxy (v, k))) Map.lookup
  alter :: forall h k a. Map.Ord k => GWDiff r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = GWDiff $ \(_ :: Proxy v) -> withDict (Map.diffOrd (Proxy :: Proxy (v, k))) Map.alter
  mapMap = GWDiff $ M.const Map.mapMap

instance Bimap r => Bimap (GWDiff r) where

instance Lang r => Lang (GWDiff r) where
  fix = GWDiff $ M.const fix
  left = GWDiff $ M.const left
  right = GWDiff $ M.const right
  sumMatch = GWDiff $ M.const sumMatch
  unit = GWDiff $ M.const unit
  exfalso = GWDiff $ M.const exfalso
  ioRet = GWDiff $ M.const ioRet
  ioBind = GWDiff $ M.const ioBind
  nil = GWDiff $ M.const nil
  cons = GWDiff $ M.const cons
  listMatch = GWDiff $ M.const listMatch
  ioMap = GWDiff $ M.const ioMap
  writer = GWDiff $ M.const writer
  runWriter = GWDiff $ M.const runWriter
  float2Double = GWDiff $ M.const $ bimap2 float2Double id
  double2Float = GWDiff $ M.const $ bimap2 double2Float id
  state = GWDiff $ M.const state
  runState = GWDiff $ M.const runState
  putStrLn = GWDiff $ M.const putStrLn

instance DLang r => DLang (GWDiff r) where
  infDiffApp = GWDiff $ M.const infDiffApp
  intDLang _ = intDLang @r Proxy
  litInfDiff x = GWDiff $ \_ -> litInfDiff x
  nextDiff p = GWDiff $ \_ -> nextDiff p
  infDiffGet :: forall h x. RTDiff (GWDiff r) x => GWDiff r h (InfDiff Eval () x -> x)
  infDiffGet = GWDiff $ \(p :: Proxy v) -> ((infDiffGet `com2` nextDiff p) \\ rtDiffDiff @r @v @x Proxy Proxy)
  rtDiffDiff _ p = rtDiffDiff @r Proxy p
  rtdd _ = rtdd @r Proxy

type instance RTDiff (GWDiff r) x = RTDiff r x 
type instance DiffInt (GWDiff r) = DiffInt r
type instance DiffVector (GWDiff r) v = DiffVector r v