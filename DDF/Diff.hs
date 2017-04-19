{-# LANGUAGE
  NoImplicitPrelude,
  ExplicitForAll,
  InstanceSigs,
  ScopedTypeVariables,
  TypeApplications,
  FlexibleContexts,
  UndecidableInstances,
  TypeFamilies
#-}

module DDF.Diff where

import DDF.DLang
import qualified Data.Map as M
import qualified DDF.Map as Map

instance DBI r => DBI (Diff r v) where
  z = Diff z
  s (Diff x) = Diff $ s x
  abs (Diff f) = Diff $ abs f
  app (Diff f) (Diff x) = Diff $ app f x
  hoas f = Diff $ hoas (\x -> runDiff $ f $ Diff x)
  liftEnv (Diff x) = Diff $ liftEnv x

instance Bool r => Bool (Diff r v) where
  bool x = Diff $ bool x
  ite = Diff ite

instance Char r => Char (Diff r v) where
  char = Diff . char

instance Prod r => Prod (Diff r v) where
  mkProd = Diff mkProd
  zro = Diff zro
  fst = Diff fst

instance Dual r => Dual (Diff r v) where
  dual = Diff $ dual
  runDual = Diff $ runDual

instance (Vector r v, Double r, Dual r) => Double (Diff r v) where
  double x = Diff $ mkDual2 (double x) zero
  doublePlus = Diff $ lam2 $ \l r ->
    mkDual2 (plus2 (dualOrig1 l) (dualOrig1 r)) (plus2 (dualDiff1 l) (dualDiff1 r))
  doubleMinus = Diff $ lam2 $ \l r ->
    mkDual2 (minus2 (dualOrig1 l) (dualOrig1 r)) (minus2 (dualDiff1 l) (dualDiff1 r))
  doubleMult = Diff $ lam2 $ \l r ->
    mkDual2 (mult2 (dualOrig1 l) (dualOrig1 r))
      (plus2 (mult2 (dualOrig1 l) (dualDiff1 r)) (mult2 (dualOrig1 r) (dualDiff1 l)))
  doubleDivide = Diff $ lam2 $ \l r ->
    mkDual2 (divide2 (dualOrig1 l) (dualOrig1 r))
      (divide2 (minus2 (mult2 (dualOrig1 r) (dualDiff1 l)) (mult2 (dualOrig1 l) (dualDiff1 r)))
        (mult2 (dualOrig1 r) (dualOrig1 r)))
  doubleExp = Diff $ lam $ \x -> let_2 (doubleExp1 (dualOrig1 x)) (lam $ \e -> mkDual2 e (mult2 e (dualDiff1 x)))

instance (Vector r v, Lang r) => Float (Diff r v) where
  float x = Diff $ mkDual2 (float x) zero
  floatPlus = Diff $ lam2 $ \l r ->
    mkDual2 (plus2 (dualOrig1 l) (dualOrig1 r)) (plus2 (dualDiff1 l) (dualDiff1 r))
  floatMinus = Diff $ lam2 $ \l r ->
    mkDual2 (minus2 (dualOrig1 l) (dualOrig1 r)) (minus2 (dualDiff1 l) (dualDiff1 r))
  floatMult = Diff $ lam2 $ \l r ->
    mkDual2 (mult2 (float2Double1 (dualOrig1 l)) (dualOrig1 r))
      (plus2 (mult2 (float2Double1 (dualOrig1 l)) (dualDiff1 r)) (mult2 (float2Double1 (dualOrig1 r)) (dualDiff1 l)))
  floatDivide = Diff $ lam2 $ \l r ->
    mkDual2 (divide2 (dualOrig1 l) (float2Double1 (dualOrig1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (dualOrig1 r)) (dualDiff1 l)) (mult2 (float2Double1 (dualOrig1 l)) (dualDiff1 r)))
        (float2Double1 (mult2 (float2Double1 (dualOrig1 r)) (dualOrig1 r))))
  floatExp = Diff (lam $ \x -> let_2 (floatExp1 (dualOrig1 x)) (lam $ \e -> mkDual2 e (mult2 (float2Double1 e) (dualDiff1 x))))

instance Option r => Option (Diff r v) where
  nothing = Diff nothing
  just = Diff just
  optionMatch = Diff optionMatch

instance Map.Map r => Map.Map (Diff r v) where
  empty = Diff Map.empty
  singleton = Diff Map.singleton
  lookup :: forall h k a. Map.Ord k => Diff r v h (k -> M.Map k a -> Maybe a)
  lookup = withDict (Map.diffOrd (Proxy :: Proxy (v, k))) (Diff Map.lookup)
  alter :: forall h k a. Map.Ord k => Diff r v h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = withDict (Map.diffOrd (Proxy :: Proxy (v, k))) (Diff Map.alter)
  mapMap = Diff Map.mapMap

instance Bimap r => Bimap (Diff r v) where

instance Unit r => Unit (Diff r v) where
  unit = Diff unit

instance (Vector r v, Lang r) => Lang (Diff r v) where
  fix = Diff fix
  left = Diff left
  right = Diff right
  sumMatch = Diff sumMatch
  exfalso = Diff exfalso
  ioRet = Diff ioRet
  ioBind = Diff ioBind
  nil = Diff nil
  cons = Diff cons
  listMatch = Diff listMatch
  ioMap = Diff ioMap
  writer = Diff writer
  runWriter = Diff runWriter
  float2Double = Diff $ bimap2 float2Double id
  double2Float = Diff $ bimap2 double2Float id
  state = Diff state
  runState = Diff runState
  putStrLn = Diff putStrLn

instance (Vector r v, DLang r) => DLang (Diff r v) where
