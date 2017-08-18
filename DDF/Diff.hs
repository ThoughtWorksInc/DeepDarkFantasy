{-# LANGUAGE
  NoImplicitPrelude,
  ExplicitForAll,
  InstanceSigs,
  ScopedTypeVariables,
  TypeApplications,
  FlexibleContexts,
  UndecidableInstances,
  TypeFamilies,
  MultiParamTypeClasses,
  TypeOperators,
  DataKinds,
  FlexibleInstances,
  UndecidableSuperClasses
#-}

module DDF.Diff where

import DDF.Lang
import qualified Prelude as M
import qualified Data.Map as M
import qualified DDF.Map as Map
import qualified Data.Bimap as M
import qualified DDF.Meta.Dual as M
import qualified DDF.VectorTF as VTF
import qualified DDF.Meta.DiffWrapper as M.DW
import qualified Data.Functor.Foldable as M
import qualified DDF.Meta.FreeVector as M
import qualified DDF.Meta.VectorTF as M.VTF

class ObjOrd r (DiffType v x) => ObjOrdCDiff r v x
instance ObjOrd r (DiffType v x) => ObjOrdCDiff r v x
type instance ObjOrdC (Diff r v) = ObjOrdCDiff r v

type instance DiffType v (l -> r) = DiffType v l -> DiffType v r
instance DBI r => DBI (Diff r v) where
  z = Diff z
  s (Diff x) = Diff $ s x
  abs (Diff f) = Diff $ abs f
  app (Diff f) (Diff x) = Diff $ app f x
  hoas f = Diff $ hoas (\x -> runDiff $ f $ Diff x)

type instance DiffType v M.Bool = M.Bool
instance Bool r => Bool (Diff r v) where
  bool x = Diff $ bool x
  ite = Diff ite

type instance DiffType v M.Char = M.Char
instance Char r => Char (Diff r v) where
  char = Diff . char

type instance DiffType v (l, r) = (DiffType v l, DiffType v r)
instance Prod r => Prod (Diff r v) where
  mkProd = Diff mkProd
  zro = Diff zro
  fst = Diff fst

type instance DiffType v (M.Dual l r) = M.Dual (DiffType v l) (DiffType v r)
instance Dual r => Dual (Diff r v) where
  dual = Diff $ dual
  runDual = Diff $ runDual

type instance DiffType v M.Double = M.Dual M.Double v
instance (Vector r v, Lang r) => Double (Diff r v) where
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
  doubleEq = Diff $ lam2 $ \l r -> doubleEq2 (dualOrig1 l) (dualOrig1 r)

type instance DiffType v M.Float = M.Dual M.Float v
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

type instance DiffType v (Maybe l) = Maybe (DiffType v l)
instance Option r => Option (Diff r v) where
  nothing = Diff nothing
  just = Diff just
  optionMatch = Diff optionMatch

type instance DiffType v (M.Map k val) = M.Map (DiffType v k) (DiffType v val)
instance Map.Map r => Map.Map (Diff r v) where
  empty = Diff Map.empty
  singleton = Diff Map.singleton
  lookup :: forall h k a. MetaOrd k => Diff r v h (M.Map k a -> k -> Maybe a)
  lookup = withDict (diffOrd (Proxy :: Proxy (v, k))) (Diff Map.lookup)
  alter :: forall h k a. MetaOrd k => Diff r v h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = withDict (Map.diffOrd (Proxy :: Proxy (v, k))) (Diff Map.alter)
  mapMap = Diff Map.mapMap
  unionWith :: forall h k a. MetaOrd k => Diff r v h ((a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
  unionWith = withDict (Map.diffOrd (Proxy :: Proxy (v, k))) (Diff Map.unionWith)

type instance DiffType v (M.Bimap a b) = M.Bimap (DiffType v a) (DiffType v b)
instance Bimap r => Bimap (Diff r v) where
  size = Diff size
  toMapL = Diff toMapL
  toMapR = Diff toMapR
  lookupL :: forall h a b. (MetaOrd a, MetaOrd b) => Diff r v h (M.Bimap a b -> a -> Maybe b)
  lookupL = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff lookupL))
  lookupR :: forall h a b. (MetaOrd a, MetaOrd b) => Diff r v h (M.Bimap a b -> b -> Maybe a)
  lookupR = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff lookupR))
  empty = Diff empty
  singleton = Diff singleton
  insert :: forall h a b. (MetaOrd a, MetaOrd b) => Diff r v h ((a, b) -> M.Bimap a b -> M.Bimap a b)
  insert = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff insert))
  updateL :: forall h a b. (MetaOrd a, MetaOrd b) => Diff r v h ((b -> Maybe b) -> a -> M.Bimap a b -> M.Bimap a b)
  updateL = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff updateL))
  updateR :: forall h a b. (MetaOrd a, MetaOrd b) => Diff r v h ((a -> Maybe a) -> b -> M.Bimap a b -> M.Bimap a b)
  updateR = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) (withDict (Map.diffOrd (Proxy :: Proxy (v, b))) (Diff updateR))

type instance DiffType v () = ()
instance Unit r => Unit (Diff r v) where
  unit = Diff unit

type instance DiffType v (M.Either l r) = M.Either (DiffType v l) (DiffType v r)
instance Sum r => Sum (Diff r v) where
  left = Diff left
  right = Diff right
  sumMatch = Diff sumMatch

instance Int r => ObjOrd (Diff r v) M.Int where
  cmp = Diff cmp

instance Int r => Int (Diff r v) where
  int = Diff . int
  pred = Diff pred

instance Y r => Y (Diff r v) where
  y = Diff y

type instance DiffType v (M.IO l) = M.IO (DiffType v l)
instance IO r => IO (Diff r v) where
  putStrLn = Diff putStrLn

type instance DiffType v [l] = [DiffType v l]
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

instance (ObjOrd r (M.VTF.VectorTF (DiffType v a) (DiffType v b)), Ordering r) => ObjOrd (Diff r v) (M.VTF.VectorTF a b) where
 cmp = Diff cmp

instance (ObjOrd2 r M.VTF.VectorTF (ObjOrd r) (ObjOrd r), Ordering r) =>
  ObjOrd2 (Diff r v) M.VTF.VectorTF (ObjOrd (Diff r v)) (ObjOrd (Diff r v)) where
    objOrd2 _ _ (_ :: Proxy a) (_ :: Proxy b) =
      withDict (objOrd2 @r @M.VTF.VectorTF
        (Proxy @(ObjOrd r))
        (Proxy @(ObjOrd r))
        (Proxy @(DiffType v a))
        (Proxy @(DiffType v b)))
        Dict

instance (Vector r v, Lang r) => VTF.VectorTF (Diff r v) where
  zero = Diff VTF.zero
  basis = Diff VTF.basis
  plus = Diff VTF.plus
  mult = Diff $ VTF.mult `com2` dualOrig
  vtfMatch = Diff $ lam4 $ \ze b p m -> VTF.vtfMatch4 ze b p $ lam $ \x -> app m (mkDual2 x zero)

type instance DiffType v (M.DW.DiffWrapper a x) = M.DW.DiffWrapper (v ': a) x
instance DiffWrapper r => DiffWrapper (Diff r v) where
  diffWrapper = Diff diffWrapper
  runDiffWrapper = Diff runDiffWrapper

type instance DiffType v (M.Fix f) = M.DW.DiffWrapper '[v] (f (M.Fix f))
instance DiffWrapper r => Fix (Diff r v) where
  fix = Diff diffWrapper
  runFix = Diff runDiffWrapper

type instance DiffType v (M.FreeVector a b) = M.FreeVector (DiffType v a) (DiffType v b)
instance FreeVector r => FreeVector (Diff r v) where
  freeVector = Diff freeVector
  runFreeVector = Diff runFreeVector

type instance DiffType v Void = Void
type instance DiffType v (Writer l r) = Writer (DiffType v l) (DiffType v r)
type instance DiffType v (State l r) = State (DiffType v l) (DiffType v r)
instance (Vector r v, Lang r) => Lang (Diff r v) where
  exfalso = Diff exfalso
  writer = Diff writer
  runWriter = Diff runWriter
  float2Double = Diff $ bimap2 float2Double id
  double2Float = Diff $ bimap2 double2Float id
  state = Diff state
  runState = Diff runState

instance MetaOrd () where
  diffOrd _ = Dict

instance MetaOrd a => MetaOrd [a] where
  diffOrd (_ :: Proxy (v, [a])) = withDict (Map.diffOrd (Proxy :: Proxy (v, a))) Dict

instance MetaOrd l => MetaOrd (M.Dual l r) where
  diffOrd (_ :: Proxy (v, M.Dual l r)) = withDict (Map.diffOrd (Proxy :: Proxy (v, l))) Dict

instance MetaOrd M.Double where
  diffOrd _ = Dict

instance MetaOrd M.Float where
  diffOrd _ = Dict

type instance DiffType v M.Ordering = M.Ordering

instance Ordering r => Ordering (Diff r v) where
  ordering = Diff . ordering
  sel = Diff sel