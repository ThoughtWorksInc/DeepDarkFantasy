{-# LANGUAGE
  RankNTypes,
  ConstraintKinds,
  NoImplicitPrelude,
  KindSignatures,
  TypeOperators,
  MultiParamTypeClasses,
  FlexibleContexts,
  UndecidableInstances,
  FlexibleInstances,
  TypeFamilies,
  UndecidableSuperClasses,
  TemplateHaskell,
  TypeApplications,
  ScopedTypeVariables,
  PartialTypeSignatures,
  AllowAmbiguousTypes,
  InstanceSigs
#-}

module DDF.TermGen (module DDF.TermGen, module DDF.Lang) where

import DDF.Lang
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF
import qualified Prelude as M
import Language.Haskell.TH
import qualified DDF.Meta.Dual as M
import qualified Data.Bimap as M
import qualified DDF.Meta.VectorTF as M.VTF
import qualified Data.Map as M

type instance OrdC (Term c) = TermOrd c
class TermOrd (c :: (* -> * -> *) -> Constraint) x where
  termOrdDict :: c r => Dict (Ord r x)

instance SubL c Int => TermOrd c M.Int where
  termOrdDict :: forall r. c r => Dict (Ord r M.Int)
  termOrdDict = Dict \\ sub @c @Int @r

instance SubL c Double => TermOrd c M.Double where
  termOrdDict :: forall r. c r => Dict (Ord r M.Double)
  termOrdDict = Dict \\ sub @c @Double @r

instance (SubL c Dual, TermOrd c x) => TermOrd c (M.Dual x y) where
  termOrdDict :: forall r. c r => Dict (Ord r (M.Dual x y))
  termOrdDict = withDict (termOrdDict @c @x @r) Dict \\ sub @c @Dual @r

instance (SubL c VTF.VectorTF, TermOrd c t, TermOrd c f) => TermOrd c (M.VTF.VectorTF t f) where
  termOrdDict :: forall r. c r => Dict (Ord r (M.VTF.VectorTF t f))
  termOrdDict = (withDict (termOrdDict @c @t @r) $ withDict (termOrdDict @c @f @r) Dict) \\ sub @c @VTF.VectorTF @r

type family SubLC (l :: (* -> * -> *) -> Constraint) (r :: (* -> * -> *) -> Constraint) :: Constraint

class SubLC l r => SubL l r where
  sub :: forall repr. l repr :- r repr

newtype Term c h s = Term { runTerm :: forall r. c r => r h s }

mkT :: forall r l h s. (SubL l r) =>
  (forall (repr :: * -> * -> *). (l repr, r repr) => repr h s) ->
  Term l h s

mkT f = Term k
  where
    k :: forall repr. l repr => repr h s
    k = f @repr \\ sub @l @r @repr

mkT1 :: forall r a l h s. (SubL l r, OrdWC (Term l) a) =>
  (forall (repr :: * -> * -> *). (l repr, r repr, Ord repr a) => repr h s) ->
  Term l h s

mkT1 f = Term k
  where
    k :: forall repr. l repr => repr h s
    k = withDict (termOrdDict @l @a @repr) (f @repr \\ sub @l @r @repr)

mkT2 :: forall r a b l h s. (SubL l r, OrdWC (Term l) a, OrdWC (Term l) b) =>
  (forall (repr :: * -> * -> *). (l repr, r repr, Ord repr a, Ord repr b) => repr h s) ->
  Term l h s

mkT2 f = Term k
  where
    k :: forall repr. l repr => repr h s
    k = withDict (termOrdDict @l @a @repr) $ withDict (termOrdDict @l @b @repr) (f @repr \\ sub @l @r @repr)

type instance SubLC c DBI = ()

instance SubL c DBI => DBI (Term c) where
  z = mkT @DBI z
  s (Term x) = mkT @DBI (s x)
  abs (Term x) = mkT @DBI (abs x)
  app (Term f) (Term x) = mkT @DBI (app f x)

type instance SubLC c Bool = SubL c DBI

instance SubL c Bool => Bool (Term c) where
  ite = mkT @Bool ite
  bool x = mkT @Bool (bool x)

type instance SubLC c Int = SubL c Ordering

instance SubL c Int => Int (Term c) where
  pred = mkT @Int pred
  int x = mkT @Int (int x)
  intCmp = mkT @Int cmp

type instance SubLC c Fix = SubL c DBI

instance SubL c Fix => Fix (Term c) where
  fix = mkT @Fix fix
  runFix = mkT @Fix runFix

type instance SubLC c FreeVector = SubL c DBI

instance SubL c FreeVector => FreeVector (Term c) where
  freeVector = mkT @FreeVector freeVector
  runFreeVector = mkT @FreeVector runFreeVector

type instance SubLC c DiffWrapper = SubL c DBI

instance SubL c DiffWrapper => DiffWrapper (Term c) where
  diffWrapper = mkT @DiffWrapper diffWrapper
  runDiffWrapper = mkT @DiffWrapper runDiffWrapper

type instance SubLC c Char = SubL c DBI

instance SubL c Char => Char (Term c) where
  char x = mkT @Char (char x)

termOrd :: forall c a r. (Ord (Term c) a, c r) => Dict (Ord r a)
termOrd = withDict (getOrdC @(Term c) @a Proxy) (termOrdDict @c @a @r)

termOrd2 :: forall c a b r. (Ord (Term c) a, Ord (Term c) b, c r) => Dict (Ord r a, Ord r b)
termOrd2 = withDict (termOrd @c @a @r) $ withDict (termOrd @c @b @r) Dict

type instance SubLC c Bimap = (SubL c Int, SubL c Map.Map)
instance SubL c Bimap => Bimap (Term c) where
  size = mkT @Bimap size
  empty = mkT @Bimap empty
  toMapL = mkT @Bimap toMapL
  toMapR = mkT @Bimap toMapR
  singleton = mkT @Bimap singleton
  lookupL' :: forall h a b. (OrdWC (Term c) a, OrdWC (Term c) b) => Term c h (M.Bimap a b -> a -> Maybe b)
  lookupL' = mkT2 @Bimap @a @b lookupL
  lookupR' :: forall h a b. (OrdWC (Term c) a, OrdWC (Term c) b) => Term c h (M.Bimap a b -> b -> Maybe a)
  lookupR' = mkT2 @Bimap @a @b lookupR
  updateL' :: forall h a b. (OrdWC (Term c) a, OrdWC (Term c) b) => Term c h ((b -> Maybe b) -> a -> M.Bimap a b -> M.Bimap a b)
  updateL' = mkT2 @Bimap @a @b updateL
  updateR' :: forall h a b. (OrdWC (Term c) a, OrdWC (Term c) b) => Term c h ((a -> Maybe a) -> b -> M.Bimap a b -> M.Bimap a b)
  updateR' = mkT2 @Bimap @a @b updateR
  insert' :: forall h a b. (OrdWC (Term c) a, OrdWC (Term c) b) => Term c h ((a, b) -> M.Bimap a b -> M.Bimap a b)
  insert' = mkT2 @Bimap @a @b insert

type instance SubLC c Float = SubL c DBI
instance SubL c Float => Float (Term c) where
  float x = mkT @Float (float x) 
  floatExp = mkT @Float floatExp
  floatPlus = mkT @Float floatPlus
  floatMult = mkT @Float floatMult
  floatMinus = mkT @Float floatMinus
  floatDivide = mkT @Float floatDivide

type instance SubLC c Double = SubL c Ordering
instance SubL c Double => Double (Term c) where
  double x = mkT @Double (double x)
  doubleCmp = mkT @Double doubleCmp
  doubleExp = mkT @Double doubleExp
  doublePlus = mkT @Double doublePlus  
  doubleMult = mkT @Double doubleMult
  doubleMinus = mkT @Double doubleMinus
  doubleDivide = mkT @Double doubleDivide

type instance SubLC c Dual = SubL c Prod

instance SubL c Dual => Dual (Term c) where
  dual = mkT @Dual dual
  runDual = mkT @Dual runDual
  dualGetOrdC :: forall x y. Ord (Term c) x :- TermOrd c (M.Dual x y)
  dualGetOrdC = Sub (withDict (getOrdC @(Term c) @x Proxy) Dict)

type instance SubLC c Unit = SubL c DBI

instance SubL c Unit => Unit (Term c) where
  unit = mkT @Unit unit

type instance SubLC c Sum = SubL c DBI

instance SubL c Sum => Sum (Term c) where
  left = mkT @Sum left
  right = mkT @Sum right
  sumMatch = mkT @Sum sumMatch

type instance SubLC c IO = (SubL c Unit, SubL c Char, SubL c List)

instance SubL c IO => IO (Term c) where
  putStrLn = mkT @IO putStrLn
  ioMap = mkT @IO map
  ioAP = mkT @IO ap
  ioPure = mkT @IO pure
  ioJoin = mkT @IO join
  ioBind = mkT @IO bind

type instance SubLC c List = SubL c Y
instance SubL c List => List (Term c) where
  nil = mkT @List nil
  cons = mkT @List cons
  listMatch = mkT @List listMatch

type instance SubLC c Prod = SubL c Ordering
instance SubL c Prod => Prod (Term c) where
  zro = mkT @Prod zro
  fst = mkT @Prod fst
  mkProd = mkT @Prod mkProd

type instance SubLC c Y = SubL c DBI

instance SubL c Y => Y (Term c) where
  y = mkT @Y y

type instance SubLC c Map.Map = (SubL c Prod, SubL c Option)
instance SubL c Map.Map => Map.Map (Term c) where
  empty = mkT @Map.Map Map.empty
  mapMap = mkT @Map.Map Map.mapMap
  alter' :: forall h k a. OrdWC (Term c) k => Term c h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter' = mkT1 @Map.Map @k Map.alter
  unionWithKey' :: forall h k a. OrdWC (Term c) k => Term c h ((k -> a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
  unionWithKey' = mkT1 @Map.Map @k Map.unionWithKey
  lookup' :: forall h k a. OrdWC (Term c) k => Term c h (M.Map k a -> k -> Maybe a)
  lookup' = mkT1 @Map.Map @k Map.lookup

type instance SubLC c VTF.VectorTF = (SubL c Ordering, SubL c Double)
instance SubL c VTF.VectorTF => VTF.VectorTF (Term c) where
  zero = mkT @VTF.VectorTF VTF.zero
  plus = mkT @VTF.VectorTF VTF.plus
  mult = mkT @VTF.VectorTF VTF.mult
  basis = mkT @VTF.VectorTF VTF.basis
  vtfMatch = mkT @VTF.VectorTF VTF.vtfMatch
  vtfCmp = mkT @VTF.VectorTF VTF.vtfCmp
  vtfGetOrdC :: forall t f. (Ord (Term c) t, Ord (Term c) f) :- TermOrd c (M.VTF.VectorTF t f)
  vtfGetOrdC = Sub (withDict (getOrdC @(Term c) @t Proxy) $ withDict (getOrdC @(Term c) @f Proxy) Dict)
  
type instance SubLC c Option = SubL c DBI
instance SubL c Option => Option (Term c) where
  just = mkT @Option just
  nothing = mkT @Option nothing
  optionMatch = mkT @Option optionMatch

type instance SubLC c Ordering = SubL c Bool
instance SubL c Ordering => Ordering (Term c) where
  sel = mkT @Ordering sel
  ordering x = mkT @Ordering (ordering x)

genInstance :: Q [Dec]
genInstance =
  M.mapM gen [
    ''DBI,
    ''Double,
    ''Bool,
    ''Lang,
    ''Fix,
    ''Int,
    ''Char,
    ''Float,
    ''VTF.VectorTF,
    ''Map.Map,
    ''Bimap,
    ''Prod,
    ''IO,
    ''Unit,
    ''Option,
    ''Sum,
    ''List,
    ''Y,
    ''Dual,
    ''DiffWrapper,
    ''FreeVector,
    ''Ordering]
    where
      gen n = M.return $
        InstanceD
          M.Nothing
          []
          (AppT (AppT (ConT ''SubL) (ConT ''Lang)) (ConT n))
          [ValD (VarP 'sub) (NormalB (AppE (ConE 'Sub) (ConE 'Dict))) []]

type instance SubLC c Lang = (
  SubL c Fix,
  SubL c Int,
  SubL c Float,
  SubL c Double,
  SubL c Bimap,
  SubL c IO,
  SubL c Sum,
  SubL c Dual,
  SubL c DiffWrapper,
  SubL c FreeVector,
  SubL c VTF.VectorTF,
  SubL c Ordering)

instance SubL c Lang => Lang (Term c) where
  state = mkT @Lang state
  writer = mkT @Lang writer
  exfalso = mkT @Lang exfalso
  runState = mkT @Lang runState
  runWriter = mkT @Lang runWriter
  float2Double = mkT @Lang float2Double
  double2Float = mkT @Lang double2Float
