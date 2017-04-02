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

instance Lang r => Bool (ImpW r) where
  bool = NoImpW . bool
  ite = NoImpW ite

instance Lang repr => DBI (ImpW repr) where
  z = NoImpW z
  s :: forall a h b. ImpW repr h b -> ImpW repr (a, h) b
  s (ImpW x) = work x
    where
      work :: Weight w => repr h (w -> b) -> ImpW repr (a, h) b
      work x = ImpW (s x)
  s (NoImpW x) = NoImpW (s x)
  app (ImpW f) (ImpW x) = ImpW (lam $ \p -> app (app (conv f) (zro1 p)) (app (conv x) (fst1 p)))
  app (NoImpW f) (NoImpW x) = NoImpW (app f x)
  app (ImpW f) (NoImpW x) = ImpW (lam $ \w -> app2 (conv f) w (conv x))
  app (NoImpW f) (ImpW x) = ImpW (lam $ \w -> app (conv f) (app (conv x) w))
  abs (ImpW f) = ImpW (flip1 $ abs f)
  abs (NoImpW x) = NoImpW (abs x)

instance Lang repr => Lang (ImpW repr) where
  nil = NoImpW nil
  cons = NoImpW cons
  listMatch = NoImpW listMatch
  zro = NoImpW zro
  fst = NoImpW fst
  mkProd = NoImpW mkProd
  ioRet = NoImpW ioRet
  ioMap = NoImpW ioMap
  ioBind = NoImpW ioBind
  unit = NoImpW unit
  nothing = NoImpW nothing
  just = NoImpW just
  optionMatch = NoImpW optionMatch
  exfalso = NoImpW exfalso
  fix = NoImpW fix
  left = NoImpW left
  right = NoImpW right
  sumMatch = NoImpW sumMatch
  writer = NoImpW writer
  runWriter = NoImpW runWriter
  double = NoImpW . double
  doubleExp = NoImpW doubleExp
  doublePlus = NoImpW doublePlus
  doubleMinus = NoImpW doubleMinus
  doubleMult = NoImpW doubleMult
  doubleDivide = NoImpW doubleDivide
  float = NoImpW . float
  floatExp = NoImpW floatExp
  floatPlus = NoImpW floatPlus
  floatMinus = NoImpW floatMinus
  floatMult = NoImpW floatMult
  floatDivide = NoImpW floatDivide
  float2Double = NoImpW float2Double
  double2Float = NoImpW double2Float
  state = NoImpW state
  runState = NoImpW runState
