{-# LANGUAGE
  NoImplicitPrelude,
  KindSignatures,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  NoMonomorphismRestriction,
  TypeFamilies,
  MultiParamTypeClasses,
  TypeApplications,
  FlexibleContexts,
  TypeOperators,
  PartialTypeSignatures,
  FlexibleInstances
#-}

module DDF.LiftDiff where

import DDF.DLang
import DDF.InfDiff ()
import DDF.Eval ()
import qualified DDF.Map as Map
import qualified Data.Map as M
import qualified Prelude as M
import qualified DDF.Meta.Dual as M

newtype LiftInfDiff r h x = LiftInfDiff {runLiftInfDiff :: r (LiftDiff h) (LiftDiff x)}

instance DBI r => DBI (LiftInfDiff r) where
  z = LiftInfDiff z
  s (LiftInfDiff x) = LiftInfDiff $ s x
  abs (LiftInfDiff x) = LiftInfDiff $ abs x
  app (LiftInfDiff f) (LiftInfDiff x) = LiftInfDiff $ app f x
  liftEnv (LiftInfDiff x) = LiftInfDiff $ liftEnv x

instance Dual r => Dual (LiftInfDiff r) where
  dual = LiftInfDiff dual
  runDual = LiftInfDiff runDual

instance Option r => Option (LiftInfDiff r) where
  nothing = LiftInfDiff nothing
  just = LiftInfDiff just
  optionMatch = LiftInfDiff optionMatch

instance Map.Map r => Map.Map (LiftInfDiff r) where
  empty = LiftInfDiff Map.empty
  singleton = LiftInfDiff Map.singleton
  lookup :: forall h k a. Map.Ord k => LiftInfDiff r h (k -> M.Map k a -> Maybe a)
  lookup = withDict (Map.liftDiffOrd (Proxy :: Proxy k)) (LiftInfDiff Map.lookup)
  alter :: forall h k a. Map.Ord k => LiftInfDiff r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = withDict (Map.liftDiffOrd (Proxy :: Proxy k)) (LiftInfDiff Map.alter)
  mapMap = LiftInfDiff Map.mapMap

instance Bimap r => Bimap (LiftInfDiff r) where

instance Prod r => Prod (LiftInfDiff r) where
  mkProd = LiftInfDiff mkProd
  zro = LiftInfDiff zro
  fst = LiftInfDiff fst

instance Bool r => Bool (LiftInfDiff r) where
  bool b = LiftInfDiff $ bool b
  ite = LiftInfDiff ite

instance Char r => Char (LiftInfDiff r) where
  char c = LiftInfDiff $ char c

instance DLang r => Float (LiftInfDiff r) where
  float d = LiftInfDiff $ litInfDiff $ intDLang @r Proxy `withDict` float d
  floatPlus = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` floatPlus
  floatMinus = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` floatMinus
  floatMult = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` floatMult
  floatDivide = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` floatDivide
  floatExp = LiftInfDiff $ infDiffApp1 $ litInfDiff $ intDLang @r Proxy `withDict` floatExp

instance DLang r => Double (LiftInfDiff r) where
  double d = LiftInfDiff $ litInfDiff $ intDLang @r Proxy `withDict` double d
  doublePlus = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` doublePlus
  doubleMinus = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` doubleMinus
  doubleMult = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` doubleMult
  doubleDivide = LiftInfDiff $ infDiffAppApp1 $ litInfDiff $ intDLang @r Proxy `withDict` doubleDivide
  doubleExp = LiftInfDiff $ infDiffApp1 $ litInfDiff $ intDLang @r Proxy `withDict` doubleExp

instance DLang r => Lang (LiftInfDiff r) where
  fix = LiftInfDiff fix
  left = LiftInfDiff left
  right = LiftInfDiff right
  sumMatch = LiftInfDiff sumMatch
  unit = LiftInfDiff unit
  exfalso = LiftInfDiff exfalso
  ioRet = LiftInfDiff ioRet
  ioBind = LiftInfDiff ioBind
  ioMap = LiftInfDiff ioMap
  nil = LiftInfDiff nil
  cons = LiftInfDiff cons
  listMatch = LiftInfDiff listMatch
  writer = LiftInfDiff writer
  runWriter = LiftInfDiff runWriter
  state = LiftInfDiff state
  runState = LiftInfDiff runState
  putStrLn = LiftInfDiff putStrLn
  float2Double = LiftInfDiff $ infDiffApp1 $ litInfDiff $ intDLang @r Proxy `withDict` float2Double
  double2Float = LiftInfDiff $ infDiffApp1 $ litInfDiff $ intDLang @r Proxy `withDict` double2Float

instance DLang r => DLang (LiftInfDiff r) where
  infDiffApp = LiftInfDiff id
  nextDiff = nextDiffAux where
    nextDiffAux :: forall v h x. (DiffVector (LiftInfDiff r) v, RTDiff (LiftInfDiff r) x, RTDiff (LiftInfDiff r) v) =>
      Proxy v -> LiftInfDiff r h (InfDiff Eval () x -> InfDiff Eval () (Diff v x))
    nextDiffAux (_ :: Proxy v) = LiftInfDiff $ diffLiftDiff @r @x @v Proxy
  litInfDiff (LiftInfDiff x) = LiftInfDiff $ liftEnv x
  intDLang _ = Dict
  rtDiffDiff _ (p :: Proxy (v, x)) = Sub $ (diffLiftDiffDiff @r (Proxy :: Proxy (v, x)) `withDict` (Dict \\ rtDiffDiff @r Proxy p))
  infDiffGet = LiftInfDiff id
  rtdd _ = rtdd @r Proxy `withDict` Dict

type instance DiffInt (LiftInfDiff r) = LiftInfDiff r
type instance RTDiff (LiftInfDiff r) x = (DiffLiftDiff r x, RTDiff r x)
type instance DiffVector (LiftInfDiff r) v = (DiffLiftDiff r v, DiffVector r v, InfDiffLiftDiff r v)

instance DLang r => DiffLiftDiff r M.Double where
  unDiffLiftDiff _ = dualOrig
  diffLiftDiff (_ :: Proxy (v, M.Double)) = rtdd @r Proxy `withDict` (hoas $ \x ->
    mkDual2 x (infDiffLiftDiff1 $ infDiffApp2 (litInfDiff $ intDLang @r Proxy `withDict` dualDiff) $ nextDiff1 (Proxy :: Proxy v) x))
  diffLiftDiffDiff _ = Dict

instance (DLang r, DiffLiftDiff r x, DiffLiftDiff r y) => DiffLiftDiff r (M.Dual x y) where
  diffLiftDiff (_ :: Proxy (v, M.Dual x y)) = bimap2 (diffLiftDiff @r @x @v Proxy) (diffLiftDiff @r @y @v Proxy)
  unDiffLiftDiff (_ :: Proxy (v, M.Dual x y)) = bimap2 (unDiffLiftDiff @r @x @v Proxy) (unDiffLiftDiff @r @y @v Proxy)
  diffLiftDiffDiff (_ :: Proxy (v, M.Dual x y)) =
    diffLiftDiffDiff @r (Proxy :: Proxy (v, x)) `withDict` (diffLiftDiffDiff @r (Proxy :: Proxy (v, y)) `withDict` Dict)

instance (DLang r, DiffLiftDiff r x, DiffLiftDiff r y) => DiffLiftDiff r (x -> y) where
  diffLiftDiff (_ :: Proxy (v, x -> y)) =
    lam $ \f -> diffLiftDiff (Proxy :: Proxy (v, y)) `com2` f `com2` unDiffLiftDiff (Proxy :: Proxy (v, x))
  unDiffLiftDiff (_ :: Proxy (v, x -> y)) =
    lam $ \f -> unDiffLiftDiff (Proxy :: Proxy (v, y)) `com2` f `com2` diffLiftDiff (Proxy :: Proxy (v, x))
  diffLiftDiffDiff (_ :: Proxy (v, x -> y)) =
    diffLiftDiffDiff @r (Proxy :: Proxy (v, x)) `withDict` (diffLiftDiffDiff @r (Proxy :: Proxy (v, y)) `withDict` Dict)

diff :: forall r v h x. (DLang r, DiffVector r v, DiffLiftDiff r v, DiffLiftDiff r x, RTDiff r x, RTDiff r v, InfDiffLiftDiff r v) =>
  Proxy v -> LiftInfDiff r h (x -> Diff v x)
diff _ = LiftInfDiff $ runLiftInfDiff $ nextDiff @_ @v @x @h Proxy

diffDD :: forall r. (DiffVector r M.Double, DLang r, RTDiff r M.Double, RTDiff r (M.Double -> M.Double), InfDiffLiftDiff r M.Double) =>
  LiftInfDiff r () ((M.Double -> M.Double) -> Diff M.Double (M.Double -> M.Double))
diffDD = diff Proxy

diffDDE :: LiftInfDiff Eval () ((M.Double -> M.Double) -> Diff M.Double (M.Double -> M.Double))
diffDDE = diffDD

instance DBI r => InfDiffLiftDiff r M.Double where
  infDiffLiftDiff = id

class InfDiffLiftDiff r x where
  infDiffLiftDiff :: forall h. r h (InfDiff Eval () x -> LiftDiff x)

infDiffLiftDiff1 = app infDiffLiftDiff

class LiftDiffInfDiff r x where
  liftDiffInfDiff :: forall h. r h (LiftDiff x -> InfDiff Eval () x)

class DiffLiftDiff (r :: * -> * -> *) x where
  diffLiftDiff :: forall v h. (RTDiff r v, DiffVector r v, DiffLiftDiff r v, InfDiffLiftDiff r v) => Proxy (v, x) -> r h (LiftDiff x -> LiftDiff (Diff v x))
  unDiffLiftDiff :: forall v h. (RTDiff r v, DiffVector r v, DiffLiftDiff r v, InfDiffLiftDiff r v) => Proxy (v, x) -> r h (LiftDiff (Diff v x) -> LiftDiff x)
  diffLiftDiffDiff :: forall v. (RTDiff r v, DiffVector r v, DiffLiftDiff r v, InfDiffLiftDiff r v) => Proxy (v, x) -> Dict (DiffLiftDiff r (Diff v x))
  