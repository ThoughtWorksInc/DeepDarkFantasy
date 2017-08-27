{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  InstanceSigs,
  ScopedTypeVariables,
  ExistentialQuantification,
  TypeFamilies,
  TypeApplications,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeOperators
#-}

module DDF.ImpW where

import DDF.Lang
import DDF.WithDiff
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF
import qualified Prelude as M
import DDF.Meta.Util
import DDF.Vector
import DDF.Meta.VectorTF as M.VTF

type instance OrdC (ImpW r) = Ord r

class ProdCon con l r where
  prodCon :: (con l, con r) :- con (l, r)

instance ProdCon Random l r where prodCon = Sub Dict

instance ProdCon RandRange l r where prodCon = Sub Dict

instance ProdCon M.Show l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Monoid repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Reify repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Vector repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (WithDiff repr) l r where prodCon = Sub Dict

class Weight w where
  weightCon :: (con (), con M.Float, con M.Double, ForallV (ProdCon con)) :- con w

instance Weight () where weightCon = Sub Dict

instance Weight M.Double where weightCon = Sub Dict

instance Weight M.Float where weightCon = Sub Dict

instance (Weight l, Weight r) => Weight (l, r) where
  weightCon :: forall con. (con (), con M.Float, con M.Double, ForallV (ProdCon con)) :- con (l, r)
  weightCon = Sub (mapDict (prodCon \\ (instV :: (ForallV (ProdCon con) :- ProdCon con l r))) (Dict \\ weightCon @l @con \\ weightCon @r @con))

runImpW :: forall r h x. Unit r => ImpW r h x -> RunImpW r h x
runImpW (ImpW x) = RunImpW x
runImpW (NoImpW x) = RunImpW (const1 x :: r h (() -> x))

data RunImpW repr h x = forall w. Weight w => RunImpW (repr h (w -> x))
data ImpW repr h x = NoImpW (repr h x) | forall w. Weight w => ImpW (repr h (w -> x))
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
  doubleCmp = NoImpW doubleCmp

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

instance Map.Map r => Map.Map (ImpW r) where
  empty = NoImpW Map.empty
  singleton' = NoImpW Map.singleton
  lookup' = NoImpW Map.lookup
  alter' = NoImpW Map.alter
  mapMap = NoImpW Map.mapMap
  unionWithKey' = NoImpW Map.unionWithKey

instance Bimap r => Bimap (ImpW r) where
  size = NoImpW size
  lookupL' = NoImpW lookupL
  lookupR' = NoImpW lookupR
  singleton = NoImpW singleton
  empty = NoImpW empty
  insert' = NoImpW insert
  toMapL = NoImpW toMapL
  toMapR = NoImpW toMapR
  updateL' = NoImpW updateL
  updateR' = NoImpW updateR

instance Dual r => Dual (ImpW r) where
  dual = NoImpW dual
  runDual = NoImpW runDual
  dualGetOrdC = Sub (getOrdC @(ImpW r) Proxy)

instance (Prod r, Unit r) => Unit (ImpW r) where
  unit = NoImpW unit

instance (Prod r, Sum r) => Sum (ImpW r) where
  left = NoImpW left
  right = NoImpW right
  sumMatch = NoImpW sumMatch

instance (Prod r, Int r) => Int (ImpW r) where
  int = NoImpW . int
  pred = NoImpW pred
  intCmp = NoImpW cmp

instance (Prod r, IO r) => IO (ImpW r) where
  putStrLn = NoImpW putStrLn
  ioMap = NoImpW map
  ioAP = NoImpW ap
  ioPure = NoImpW pure
  ioJoin = NoImpW join
  ioBind = NoImpW bind

instance (Prod r, List r) => List (ImpW r) where
  nil = NoImpW nil
  cons = NoImpW cons
  listMatch = NoImpW listMatch

instance (Prod r, Y r) => Y (ImpW r) where
  y = NoImpW y

instance (Prod r, VTF.VectorTF r) => VTF.VectorTF (ImpW r) where
  zero = NoImpW VTF.zero
  basis = NoImpW VTF.basis
  plus = NoImpW VTF.plus
  mult = NoImpW VTF.mult
  vtfMatch = NoImpW VTF.vtfMatch
  vtfCmp = NoImpW VTF.vtfCmp
  vtfGetOrdC :: forall t f. (Ord (ImpW r) t, Ord (ImpW r) f) :- Ord r (M.VTF.VectorTF t f)
  vtfGetOrdC = Sub (withDict (getOrdC @(ImpW r) @t Proxy) $ withDict (getOrdC @(ImpW r) @f Proxy) Dict)

instance (Prod r, DiffWrapper r) => DiffWrapper (ImpW r) where
  diffWrapper = NoImpW diffWrapper
  runDiffWrapper = NoImpW runDiffWrapper

instance (Prod r, Fix r) => Fix (ImpW r) where
  fix = NoImpW fix
  runFix = NoImpW runFix

instance (Prod r, FreeVector r) => FreeVector (ImpW r) where
  freeVector = NoImpW freeVector
  runFreeVector = NoImpW runFreeVector

instance (Prod r, Ordering r) => Ordering (ImpW r) where
  sel = NoImpW sel
  ordering = NoImpW . ordering

instance Lang r => Lang (ImpW r) where
  exfalso = NoImpW exfalso
  writer = NoImpW writer
  runWriter = NoImpW runWriter
  float2Double = NoImpW float2Double
  double2Float = NoImpW double2Float
  state = NoImpW state
  runState = NoImpW runState
