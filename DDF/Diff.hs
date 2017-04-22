{-# LANGUAGE
  NoImplicitPrelude,
  ExplicitForAll,
  InstanceSigs,
  ScopedTypeVariables,
  TypeApplications,
  FlexibleContexts,
  UndecidableInstances,
  TypeFamilies,
  MultiParamTypeClasses
#-}

module DDF.Diff where

import DDF.Lang
import qualified Prelude as M
import qualified Data.Map as M
import qualified DDF.Map as Map
import qualified Data.Bimap as M

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
  lookup :: forall h k a. Map.Ord k => Diff r v h (M.Map k a -> k -> Maybe a)
  lookup = withDict (Map.diffOrd (Proxy :: Proxy (v, k))) (Diff Map.lookup)
  alter :: forall h k a. Map.Ord k => Diff r v h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = withDict (Map.diffOrd (Proxy :: Proxy (v, k))) (Diff Map.alter)
  mapMap = Diff Map.mapMap

instance Bimap r => Bimap (Diff r v) where
  size = Diff size
  toMapL = Diff toMapL
  toMapR = Diff toMapR
  lookupL :: forall h a b. (Map.Ord a, Map.Ord b) => Diff r v h (M.Bimap a b -> a -> Maybe b)
  lookupL = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff lookupL))
  lookupR :: forall h a b. (Map.Ord a, Map.Ord b) => Diff r v h (M.Bimap a b -> b -> Maybe a)
  lookupR = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff lookupR))
  empty = Diff empty
  singleton = Diff singleton
  insert :: forall h a b. (Map.Ord a, Map.Ord b) => Diff r v h ((a, b) -> M.Bimap a b -> M.Bimap a b)
  insert = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff insert))
  updateL :: forall h a b. (Map.Ord a, Map.Ord b) => Diff r v h ((b -> Maybe b) -> a -> M.Bimap a b -> M.Bimap a b)
  updateL = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff updateL))
  updateR :: forall h a b. (Map.Ord a, Map.Ord b) => Diff r v h ((a -> Maybe a) -> b -> M.Bimap a b -> M.Bimap a b)
  updateR = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff updateR))

instance Unit r => Unit (Diff r v) where
  unit = Diff unit

instance Sum r => Sum (Diff r v) where
  left = Diff left
  right = Diff right
  sumMatch = Diff sumMatch

instance Int r => Int (Diff r v) where
  int = Diff . int

instance Fix r => Fix (Diff r v) where
  fix = Diff fix

instance IO r => IO (Diff r v) where
  putStrLn = Diff putStrLn

instance List r => List (Diff r v) where
  nil = Diff nil
  cons = Diff cons
  listMatch = Diff listMatch

instance Functor r M.IO => Functor (Diff r v) M.IO where
  map = Diff map

instance Applicative r M.IO => Applicative (Diff r v) M.IO where
  pure = Diff pure
  ap = Diff ap

instance Monad r M.IO => Monad (Diff r v) M.IO where
  bind = Diff bind
  join = Diff join

instance (Vector r v, Lang r) => Lang (Diff r v) where
  exfalso = Diff exfalso
  writer = Diff writer
  runWriter = Diff runWriter
  float2Double = Diff $ bimap2 float2Double id
  double2Float = Diff $ bimap2 double2Float id
  state = Diff state
  runState = Diff runState
