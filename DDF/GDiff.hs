{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  TypeFamilies,
  TypeApplications
#-}

module DDF.GDiff (module DDF.Meta.Diff) where
import DDF.DLang
import qualified Prelude as M
import DDF.Meta.Diff
import qualified Data.Map as M
import qualified DDF.Map as Map

instance DBI r => DBI (GDiff r) where
  z = GDiff (M.const z)
  s (GDiff x) = GDiff (\p -> s $ x p)
  app (GDiff f) (GDiff x) = GDiff (\p -> app (f p) (x p))
  abs (GDiff x) = GDiff (\p -> abs $ x p)
  liftEnv (GDiff x) = GDiff (\p -> liftEnv $ x p)

instance Bool r => Bool (GDiff r) where
  bool x = GDiff $ M.const $ bool x
  ite = GDiff $ M.const ite

instance Char r => Char (GDiff r) where
  char x = GDiff $ M.const $ char x

instance Prod r => Prod (GDiff r) where
  mkProd = GDiff (M.const mkProd)
  zro = GDiff $ M.const zro
  fst = GDiff $ M.const fst

instance Dual r => Dual (GDiff r) where
  dual = GDiff $ M.const $ dual
  runDual = GDiff $ M.const $ runDual

instance (Double r, Dual r) => Double (GDiff r) where
  double x = GDiff $ M.const $ mkDual2 (double x) zero
  doublePlus = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (plus2 (dualOrig1 l) (dualOrig1 r)) (plus2 (dualDiff1 l) (dualDiff1 r))
  doubleMinus = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (minus2 (dualOrig1 l) (dualOrig1 r)) (minus2 (dualDiff1 l) (dualDiff1 r))
  doubleMult = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (mult2 (dualOrig1 l) (dualOrig1 r))
      (plus2 (mult2 (dualOrig1 l) (dualDiff1 r)) (mult2 (dualOrig1 r) (dualDiff1 l)))
  doubleDivide = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (divide2 (dualOrig1 l) (dualOrig1 r))
      (divide2 (minus2 (mult2 (dualOrig1 r) (dualDiff1 l)) (mult2 (dualOrig1 l) (dualDiff1 r)))
        (mult2 (dualOrig1 r) (dualOrig1 r)))
  doubleExp = GDiff $ M.const $ lam $ \x -> let_2 (doubleExp1 (dualOrig1 x)) $ lam $ \e -> mkDual2 e (mult2 e (dualDiff1 x))

instance Lang r => Float (GDiff r) where
  float x = GDiff $ M.const $ mkDual2 (float x) zero
  floatPlus = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (plus2 (dualOrig1 l) (dualOrig1 r)) (plus2 (dualDiff1 l) (dualDiff1 r))
  floatMinus = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (minus2 (dualOrig1 l) (dualOrig1 r)) (minus2 (dualDiff1 l) (dualDiff1 r))
  floatMult = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (mult2 (float2Double1 (dualOrig1 l)) (dualOrig1 r))
      (plus2 (mult2 (float2Double1 (dualOrig1 l)) (dualDiff1 r)) (mult2 (float2Double1 (dualOrig1 r)) (dualDiff1 l)))
  floatDivide = GDiff $ M.const $ lam2 $ \l r ->
    mkDual2 (divide2 (dualOrig1 l) (float2Double1 (dualOrig1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (dualOrig1 r)) (dualDiff1 l)) (mult2 (float2Double1 (dualOrig1 l)) (dualDiff1 r)))
        (float2Double1 (mult2 (float2Double1 (dualOrig1 r)) (dualOrig1 r))))
  floatExp = GDiff $ M.const $ lam $ \x -> let_2 (floatExp1 (dualOrig1 x)) $ lam $ \e -> mkDual2 e (mult2 (float2Double1 e) (dualDiff1 x))

instance Option r => Option (GDiff r) where
  nothing = GDiff $ M.const nothing
  just = GDiff $ M.const just
  optionMatch = GDiff $ M.const optionMatch

instance Map.Map r => Map.Map (GDiff r) where
  empty = GDiff $ M.const Map.empty
  singleton = GDiff $ M.const Map.singleton
  lookup :: forall h k a. Map.Ord k => GDiff r h (k -> M.Map k a -> Maybe a)
  lookup = GDiff $ \(_ :: Proxy v) -> withDict (Map.diffOrd (Proxy :: Proxy (v, k))) Map.lookup
  alter :: forall h k a. Map.Ord k => GDiff r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = GDiff $ \(_ :: Proxy v) -> withDict (Map.diffOrd (Proxy :: Proxy (v, k))) Map.alter
  mapMap = GDiff $ M.const Map.mapMap

instance Bimap r => Bimap (GDiff r) where

instance Lang r => Lang (GDiff r) where
  fix = GDiff $ M.const fix
  left = GDiff $ M.const left
  right = GDiff $ M.const right
  sumMatch = GDiff $ M.const sumMatch
  unit = GDiff $ M.const unit
  exfalso = GDiff $ M.const exfalso
  ioRet = GDiff $ M.const ioRet
  ioBind = GDiff $ M.const ioBind
  nil = GDiff $ M.const nil
  cons = GDiff $ M.const cons
  listMatch = GDiff $ M.const listMatch
  ioMap = GDiff $ M.const ioMap
  writer = GDiff $ M.const writer
  runWriter = GDiff $ M.const runWriter
  float2Double = GDiff $ M.const $ bimap2 float2Double id
  double2Float = GDiff $ M.const $ bimap2 double2Float id
  state = GDiff $ M.const state
  runState = GDiff $ M.const runState
  putStrLn = GDiff $ M.const putStrLn

instance DLang r => DLang (GDiff r) where
  infDiffApp = GDiff $ M.const infDiffApp
  intDLang _ = intDLang @r Proxy
  litInfDiff x = GDiff $ \_ -> litInfDiff x
  nextDiff p = GDiff $ \_ -> nextDiff p
  infDiffGet :: forall h x. RTDiff (GDiff r) x => GDiff r h (InfDiff Eval () x -> x)
  infDiffGet = GDiff $ \(p :: Proxy v) -> ((infDiffGet `com2` nextDiff p) \\ rtDiffDiff @r @v @x Proxy Proxy)
  rtDiffDiff _ p = rtDiffDiff @r Proxy p
  rtdd _ = rtdd @r Proxy

type instance RTDiff (GDiff r) x = RTDiff r x 
type instance DiffInt (GDiff r) = DiffInt r
type instance DiffVector (GDiff r) v = DiffVector r v