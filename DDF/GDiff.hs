{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  TypeFamilies,
  TypeApplications,
  MultiParamTypeClasses,
  FlexibleInstances,
  FlexibleContexts
#-}

module DDF.GDiff (module DDF.Meta.Diff) where
import DDF.Lang
import qualified Prelude as M
import DDF.Meta.Diff
import DDF.Diff ()
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
  lookup = GDiff $ M.const Map.lookup
  alter = GDiff $ M.const Map.alter
  mapMap = GDiff $ M.const Map.mapMap

instance Bimap r => Bimap (GDiff r) where
  size = GDiff $ M.const size
  lookupL = GDiff $ M.const lookupL
  lookupR = GDiff $ M.const lookupR
  empty = GDiff $ M.const empty
  singleton = GDiff $ M.const singleton
  insert = GDiff $ M.const insert
  updateL = GDiff $ M.const updateL
  updateR = GDiff $ M.const updateR
  toMapL = GDiff $ M.const toMapL
  toMapR = GDiff $ M.const toMapR

instance Unit r => Unit (GDiff r) where
  unit = GDiff $ M.const unit

instance Sum r => Sum (GDiff r) where
  left = GDiff $ M.const left
  right = GDiff $ M.const right
  sumMatch = GDiff $ M.const sumMatch

instance Int r => Int (GDiff r) where
  int x = GDiff $ M.const $ int x

instance Fix r => Fix (GDiff r) where
  fix = GDiff $ M.const fix

instance Functor r M.IO => Functor (GDiff r) M.IO where
  map = GDiff $ M.const map

instance Applicative r M.IO => Applicative (GDiff r) M.IO where
  pure = GDiff $ M.const pure
  ap = GDiff $ M.const ap

instance Monad r M.IO => Monad (GDiff r) M.IO where
  bind = GDiff $ M.const bind
  join = GDiff $ M.const join

instance IO r => IO (GDiff r) where
  putStrLn = GDiff $ M.const putStrLn

instance List r => List (GDiff r) where
  nil = GDiff $ M.const nil
  cons = GDiff $ M.const cons
  listMatch = GDiff $ M.const listMatch

instance Lang r => Lang (GDiff r) where
  exfalso = GDiff $ M.const exfalso
  writer = GDiff $ M.const writer
  runWriter = GDiff $ M.const runWriter
  float2Double = GDiff $ M.const float2Double
  double2Float = GDiff $ M.const double2Float
  state = GDiff $ M.const state
  runState = GDiff $ M.const runState
