{-# LANGUAGE
  NoImplicitPrelude,
  TypeFamilies,
  TypeApplications,
  ScopedTypeVariables,
  FlexibleInstances,
  MultiParamTypeClasses
#-}

module DDF.UnHOAS where

import DDF.Lang
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

instance DBI repr => DBI (UnHOAS repr) where
  z = UnHOAS z
  s (UnHOAS x) = UnHOAS $ s x
  abs (UnHOAS x) = UnHOAS $ abs x
  app (UnHOAS f) (UnHOAS x) = UnHOAS $ app f x

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
  doubleEq = UnHOAS doubleEq

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
  unionWith = UnHOAS Map.unionWith

instance Bimap r => Bimap (UnHOAS r) where
  size = UnHOAS size
  insert = UnHOAS insert
  lookupL = UnHOAS lookupL
  toMapL = UnHOAS toMapL
  lookupR = UnHOAS lookupR
  toMapR = UnHOAS toMapR
  empty = UnHOAS empty
  singleton = UnHOAS singleton
  updateL = UnHOAS updateL
  updateR = UnHOAS updateR

instance Dual r => Dual (UnHOAS r) where
  dual = UnHOAS dual
  runDual = UnHOAS runDual

instance Unit r => Unit (UnHOAS r) where
  unit = UnHOAS unit

instance Sum r => Sum (UnHOAS r) where
  left = UnHOAS left
  right = UnHOAS right
  sumMatch = UnHOAS sumMatch

instance Int r => Int (UnHOAS r) where
  int = UnHOAS . int
  pred = UnHOAS pred
  isZero = UnHOAS isZero

instance Y r => Y (UnHOAS r) where
  y = UnHOAS y

instance Functor r x => Functor (UnHOAS r) x where
  map = UnHOAS map

instance Applicative r x => Applicative (UnHOAS r) x where
  pure = UnHOAS pure
  ap = UnHOAS ap

instance Monad r x => Monad (UnHOAS r) x where
  join = UnHOAS join
  bind = UnHOAS bind

instance IO r => IO (UnHOAS r) where
  putStrLn = UnHOAS putStrLn

instance List r => List (UnHOAS r) where
  nil = UnHOAS nil
  cons = UnHOAS cons
  listMatch = UnHOAS listMatch

instance VTF.VectorTF r => VTF.VectorTF (UnHOAS r) where
  zero = UnHOAS VTF.zero
  basis = UnHOAS VTF.basis
  plus = UnHOAS VTF.plus
  mult = UnHOAS VTF.mult
  vtfMatch = UnHOAS VTF.vtfMatch

instance DiffWrapper r => DiffWrapper (UnHOAS r) where
  diffWrapper = UnHOAS diffWrapper
  runDiffWrapper = UnHOAS runDiffWrapper

instance Fix r => Fix (UnHOAS r) where
  fix = UnHOAS fix
  runFix = UnHOAS runFix

instance FreeVector r => FreeVector (UnHOAS r) where
  freeVector = UnHOAS freeVector
  runFreeVector = UnHOAS runFreeVector

instance Lang r => Lang (UnHOAS r) where
  float2Double = UnHOAS float2Double
  exfalso = UnHOAS exfalso
  writer = UnHOAS writer
  runWriter = UnHOAS runWriter
  double2Float = UnHOAS double2Float
  state = UnHOAS state
  runState = UnHOAS runState
