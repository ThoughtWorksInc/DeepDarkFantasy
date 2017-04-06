{-# LANGUAGE NoImplicitPrelude, RankNTypes, InstanceSigs, ScopedTypeVariables, ExistentialQuantification #-}

module DDF.ImpW where

import DDF.Lang

data RunImpW repr h x = forall w. Weight w => RunImpW (repr h (w -> x))
data ImpW repr h x = NoImpW (repr h x) | forall w. Weight w => ImpW (repr h (w -> x))

runImpW :: forall repr h x. Lang repr => ImpW repr h x -> RunImpW repr h x
runImpW (ImpW x) = RunImpW x
runImpW (NoImpW x) = RunImpW (const1 x :: repr h (() -> x))

type RunImpWR repr h x = forall r. (forall w. Weight w => repr h (w -> x) -> r) -> r

runImpW2RunImpWR :: RunImpW repr h x -> RunImpWR repr h x
runImpW2RunImpWR (RunImpW x) = \f -> f x

runImpWR2RunImpW :: RunImpWR repr h x -> RunImpW repr h x
runImpWR2RunImpW f = f RunImpW

instance Prod r => DBI (ImpW r) where
  z = NoImpW z
  s :: forall a h b. ImpW r h b -> ImpW r (a, h) b
  s (ImpW w) = ImpW (s w)
  s (NoImpW x) = NoImpW (s x)
  app (ImpW f) (ImpW x) = ImpW (lam $ \p -> app (app (conv f) (zro1 p)) (app (conv x) (fst1 p)))
  app (NoImpW f) (NoImpW x) = NoImpW (app f x)
  app (ImpW f) (NoImpW x) = ImpW (lam $ \w -> app2 (conv f) w (conv x))
  app (NoImpW f) (ImpW x) = ImpW (lam $ \w -> app (conv f) (app (conv x) w))
  abs (ImpW f) = ImpW (flip1 $ abs f)
  abs (NoImpW x) = NoImpW (abs x)

instance (Prod r, Bool r) => Bool (ImpW r) where
  bool = NoImpW . bool
  ite = NoImpW ite

instance (Prod r, Char r) => Char (ImpW r) where
  char = NoImpW . char

instance Prod r => Prod (ImpW r) where
  mkProd = NoImpW mkProd
  zro = NoImpW zro
  fst = NoImpW fst
  
instance (Prod r, Double r) => Double (ImpW r) where
  double = NoImpW . double
  doubleExp = NoImpW doubleExp
  doublePlus = NoImpW doublePlus
  doubleMinus = NoImpW doubleMinus
  doubleMult = NoImpW doubleMult
  doubleDivide = NoImpW doubleDivide

instance (Prod r, Float r) => Float (ImpW r) where
  float = NoImpW . float
  floatExp = NoImpW floatExp
  floatPlus = NoImpW floatPlus
  floatMinus = NoImpW floatMinus
  floatMult = NoImpW floatMult
  floatDivide = NoImpW floatDivide

instance (Prod r, Option r) => Option (ImpW r) where
  nothing = NoImpW nothing
  just = NoImpW just
  optionMatch = NoImpW optionMatch

instance Map r => Map (ImpW r) where
  empty = NoImpW empty
  singleton = NoImpW singleton
  lookup = NoImpW lookup
  alter = NoImpW alter
  mapMap = NoImpW mapMap

instance Dual r => Dual (ImpW r) where
  dual = NoImpW dual
  runDual = NoImpW runDual

instance Lang r => Lang (ImpW r) where
  nil = NoImpW nil
  cons = NoImpW cons
  listMatch = NoImpW listMatch
  ioRet = NoImpW ioRet
  ioMap = NoImpW ioMap
  ioBind = NoImpW ioBind
  unit = NoImpW unit
  exfalso = NoImpW exfalso
  fix = NoImpW fix
  left = NoImpW left
  right = NoImpW right
  sumMatch = NoImpW sumMatch
  writer = NoImpW writer
  runWriter = NoImpW runWriter
  float2Double = NoImpW float2Double
  double2Float = NoImpW double2Float
  state = NoImpW state
  runState = NoImpW runState
  putStrLn = NoImpW putStrLn