{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  TypeFamilies,
  TypeApplications
#-}

module DDF.GDiff (module DDF.Meta.Diff) where
import DDF.Lang
import qualified Prelude as M
import DDF.Meta.Diff
import DDF.Diff ()
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
  double x = GDiff $ M.const $ double x
  doublePlus = GDiff $ M.const $ doublePlus
  doubleMinus = GDiff $ M.const $ doubleMinus
  doubleMult = GDiff $ M.const $ doubleMult
  doubleDivide = GDiff $ M.const $ doubleDivide
  doubleExp = GDiff $ M.const $ doubleExp

instance Lang r => Float (GDiff r) where
  float x = GDiff $ M.const $ float x
  floatPlus = GDiff $ M.const $ floatPlus
  floatMinus = GDiff $ M.const $ floatMinus
  floatMult = GDiff $ M.const $ floatMult
  floatDivide = GDiff $ M.const $ floatDivide
  floatExp = GDiff $ M.const $ floatExp

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

instance Unit r => Unit (GDiff r) where
  unit = GDiff $ M.const unit

instance Lang r => Lang (GDiff r) where
  fix = GDiff $ M.const fix
  left = GDiff $ M.const left
  right = GDiff $ M.const right
  sumMatch = GDiff $ M.const sumMatch
  exfalso = GDiff $ M.const exfalso
  ioRet = GDiff $ M.const ioRet
  ioBind = GDiff $ M.const ioBind
  nil = GDiff $ M.const nil
  cons = GDiff $ M.const cons
  listMatch = GDiff $ M.const listMatch
  ioMap = GDiff $ M.const ioMap
  writer = GDiff $ M.const writer
  runWriter = GDiff $ M.const runWriter
  float2Double = GDiff $ M.const float2Double
  double2Float = GDiff $ M.const double2Float
  state = GDiff $ M.const state
  runState = GDiff $ M.const runState
  putStrLn = GDiff $ M.const putStrLn
