{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleInstances,
  MultiParamTypeClasses
#-}

module DDF.UnLiftEnv where

import DDF.Lang
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

newtype UnLiftEnv r h x = UnLiftEnv {runUnLiftEnv :: r () (h -> x)}

unLiftEnv = UnLiftEnv . const1

instance (Prod r, Unit r) => DBI (UnLiftEnv r) where
  z = UnLiftEnv zro
  s (UnLiftEnv x) = UnLiftEnv $ x `com2` fst
  abs (UnLiftEnv x) = UnLiftEnv $ curry1 $ x `com2` swap
  app (UnLiftEnv f) (UnLiftEnv x) = UnLiftEnv $ scomb2 f x
  liftEnv (UnLiftEnv x) = UnLiftEnv $ x `com2` (const1 unit)

instance (Dual r, Unit r) => Dual (UnLiftEnv r) where
  dual = unLiftEnv dual
  runDual = unLiftEnv runDual

instance (Prod r, Unit r, Float r) => Float (UnLiftEnv r) where
  float = unLiftEnv . float
  floatPlus = unLiftEnv floatPlus
  floatMinus = unLiftEnv floatMinus
  floatMult = unLiftEnv floatMult
  floatDivide = unLiftEnv floatDivide
  floatExp = unLiftEnv floatExp

instance (Prod r, Unit r, Double r) => Double (UnLiftEnv r) where
  double = unLiftEnv . double
  doublePlus = unLiftEnv doublePlus
  doubleMinus = unLiftEnv doubleMinus
  doubleMult = unLiftEnv doubleMult
  doubleDivide = unLiftEnv doubleDivide
  doubleExp = unLiftEnv doubleExp

instance (Prod r, Unit r, Char r) => Char (UnLiftEnv r) where
  char = unLiftEnv . char

instance (Prod r, Unit r, Bool r) => Bool (UnLiftEnv r) where
  bool = unLiftEnv . bool
  ite = unLiftEnv ite

instance (Prod r, Unit r) => Prod (UnLiftEnv r) where
  mkProd = unLiftEnv mkProd
  zro = unLiftEnv zro
  fst = unLiftEnv fst

instance (Prod r, Unit r) => Unit (UnLiftEnv r) where
  unit = unLiftEnv unit

instance (Prod r, Unit r, Option r) => Option (UnLiftEnv r) where
  nothing = unLiftEnv nothing
  just = unLiftEnv just
  optionMatch = unLiftEnv optionMatch

instance (Unit r, Map.Map r) => Map.Map (UnLiftEnv r) where
  empty = unLiftEnv Map.empty
  singleton = unLiftEnv Map.singleton
  lookup = unLiftEnv Map.lookup
  alter = unLiftEnv Map.alter
  mapMap = unLiftEnv Map.mapMap

instance (Unit r, Bimap r) => Bimap (UnLiftEnv r) where
  size = unLiftEnv size
  insert = unLiftEnv insert
  lookupL = unLiftEnv lookupL
  lookupR = unLiftEnv lookupR
  empty = unLiftEnv empty
  singleton = unLiftEnv singleton
  toMapL = unLiftEnv toMapL
  toMapR = unLiftEnv toMapR
  updateL = unLiftEnv updateL
  updateR = unLiftEnv updateR

instance (Prod r, Unit r, Sum r) => Sum (UnLiftEnv r) where
  left = unLiftEnv left
  right = unLiftEnv right
  sumMatch = unLiftEnv sumMatch

instance (Prod r, Unit r, Int r) => Int (UnLiftEnv r) where
  int = unLiftEnv . int

instance (Unit r, Prod r, Y r) => Y (UnLiftEnv r) where
  y = unLiftEnv y

instance (Prod r, IO r) => IO (UnLiftEnv r) where
  putStrLn = unLiftEnv putStrLn

instance (Unit r, Prod r, List r) => List (UnLiftEnv r) where
  nil = unLiftEnv nil
  cons = unLiftEnv cons
  listMatch = unLiftEnv listMatch

instance (Prod r, Unit r, Functor r m) => Functor (UnLiftEnv r) m where
  map = unLiftEnv map

instance (Prod r, Unit r, Applicative r m) => Applicative (UnLiftEnv r) m where
  pure = unLiftEnv pure
  ap = unLiftEnv ap

instance (Prod r, Unit r, Monad r m) => Monad (UnLiftEnv r) m where
  bind = unLiftEnv bind
  join = unLiftEnv join

instance (Prod r, Unit r, VTF.VectorTF r) => VTF.VectorTF (UnLiftEnv r) where
  zero = unLiftEnv VTF.zero
  basis = unLiftEnv VTF.basis
  plus = unLiftEnv VTF.plus
  mult = unLiftEnv VTF.mult
  vtfMatch = unLiftEnv VTF.vtfMatch

instance (Prod r, Unit r, DiffWrapper r) => DiffWrapper (UnLiftEnv r) where
  diffWrapper = unLiftEnv diffWrapper
  runDiffWrapper = unLiftEnv runDiffWrapper

instance Lang r => Lang (UnLiftEnv r) where
  exfalso = unLiftEnv exfalso
  writer = unLiftEnv writer
  runWriter = unLiftEnv runWriter
  state = unLiftEnv state
  runState = unLiftEnv runState
  float2Double = unLiftEnv float2Double
  double2Float = unLiftEnv double2Float