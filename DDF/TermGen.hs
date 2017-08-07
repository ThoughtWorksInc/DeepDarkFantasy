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
  AllowAmbiguousTypes
#-}

module DDF.TermGen (module DDF.TermGen, module DDF.Lang) where

import DDF.Lang
import qualified DDF.Map as Map
import qualified DDF.VectorTF as VTF

import qualified Prelude as M
import Language.Haskell.TH

type family SubLC (l :: (* -> * -> *) -> Constraint) (r :: (* -> * -> *) -> Constraint) :: Constraint

class SubLC l r => SubL l r where
  sub :: forall repr. l repr :- r repr
  subP :: forall repr. Proxy repr -> l repr :- r repr
  subP _ = sub

newtype Term c h s = Term { runTerm :: forall r. c r => r h s }

mkT :: (forall r. c r => Proxy r -> r h s) -> Term c h s
mkT t = Term (t Proxy)

type instance SubLC c DBI = ()

instance SubL c DBI => DBI (Term c) where
  z = mkTerm @DBI z
  s (Term x) = mkTerm @DBI (s x)
  abs (Term x) = mkTerm @DBI (abs x)
  app (Term f) (Term x) = mkTerm @DBI (app f x)

type instance SubLC c Bool = SubL c DBI

instance SubL c Bool => Bool (Term c) where
  bool x = mkTerm @Bool (bool x)
  ite = mkTerm @Bool ite

type instance SubLC c Int = SubL c Bool

instance SubL c Int => Int (Term c) where
  int x = mkT (\p -> int x \\ subP @c @Int p)
  pred = mkT (\p -> pred \\ subP @c @Int p)
  isZero = mkT (\p -> isZero \\ subP @c @Int p)

type instance SubLC c Fix = SubL c DBI

instance SubL c Fix => Fix (Term c) where
  fix = mkT (\p -> fix \\ subP @c @Fix p)
  runFix = mkT (\p -> runFix \\ subP @c @Fix p)

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
  SubL c VTF.VectorTF)

instance SubL c Lang => Lang (Term c) where
  exfalso = mkT (\p -> exfalso \\ subP @c @Lang p)
  writer = mkT (\p -> writer \\ subP @c @Lang p)
  runWriter = mkT (\p -> runWriter \\ subP @c @Lang p)
  state = mkT (\p -> state \\ subP @c @Lang p)
  runState = mkT (\p -> runState \\ subP @c @Lang p)
  float2Double = mkT (\p -> float2Double \\ subP @c @Lang p)
  double2Float = mkT (\p -> double2Float \\ subP @c @Lang p)

type instance SubLC c FreeVector = SubL c DBI

instance SubL c FreeVector => FreeVector (Term c) where
  freeVector = mkT (\p -> freeVector \\ subP @c @FreeVector p)
  runFreeVector = mkT (\p -> runFreeVector \\ subP @c @FreeVector p)

type instance SubLC c DiffWrapper = SubL c DBI

instance SubL c DiffWrapper => DiffWrapper (Term c) where
  diffWrapper = mkT (\p -> diffWrapper \\ subP @c @DiffWrapper p)
  runDiffWrapper = mkT (\p -> runDiffWrapper \\ subP @c @DiffWrapper p)

type instance SubLC c Char = SubL c DBI

instance SubL c Char => Char (Term c) where
  char x = mkT (\p -> char x \\ subP @c @Char p)

type instance SubLC c Bimap = (SubL c Int, SubL c Map.Map)

instance SubL c Bimap => Bimap (Term c) where
  size = mkT (\p -> size \\ subP @c @Bimap p)
  lookupL = mkT (\p -> lookupL \\ subP @c @Bimap p)
  lookupR = mkT (\p -> lookupR \\ subP @c @Bimap p)
  empty = mkT (\p -> empty \\ subP @c @Bimap p)
  singleton = mkT (\p -> singleton \\ subP @c @Bimap p)
  insert = mkT (\p -> insert \\ subP @c @Bimap p)
  updateL = mkT (\p -> updateL \\ subP @c @Bimap p)
  updateR = mkT (\p -> updateR \\ subP @c @Bimap p)
  toMapL = mkT (\p -> toMapL \\ subP @c @Bimap p)
  toMapR = mkT (\p -> toMapR \\ subP @c @Bimap p)

type instance SubLC c Float = SubL c DBI

instance SubL c Float => Float (Term c) where
  float x = mkT (\p -> float x \\ subP @c @Float p)
  floatPlus = mkT (\p -> floatPlus \\ subP @c @Float p)
  floatMinus = mkT (\p -> floatMinus \\ subP @c @Float p)
  floatMult = mkT (\p -> floatMult \\ subP @c @Float p)
  floatDivide = mkT (\p -> floatDivide \\ subP @c @Float p)
  floatExp = mkT (\p -> floatExp \\ subP @c @Float p)

type instance SubLC c Double = SubL c Bool

instance SubL c Double => Double (Term c) where
  double x = mkT (\p -> double x \\ subP @c @Double p)
  doublePlus = mkT (\p -> doublePlus \\ subP @c @Double p)
  doubleMinus = mkT (\p -> doubleMinus \\ subP @c @Double p)
  doubleMult = mkT (\p -> doubleMult \\ subP @c @Double p)
  doubleDivide = mkT (\p -> doubleDivide \\ subP @c @Double p)
  doubleExp = mkT (\p -> doubleExp \\ subP @c @Double p)
  doubleEq = mkT (\p -> doubleEq \\ subP @c @Double p)

type instance SubLC c Dual = SubL c Prod

instance SubL c Dual => Dual (Term c) where
  dual = mkT (\p -> dual \\ subP @c @Dual p)
  runDual = mkT (\p -> runDual \\ subP @c @Dual p)

type instance SubLC c Unit = SubL c DBI

instance SubL c Unit => Unit (Term c) where
  unit = mkT (\p -> unit \\ subP @c @Unit p)

type instance SubLC c Sum = SubL c DBI

instance SubL c Sum => Sum (Term c) where
  left = mkT (\p -> left \\ subP @c @Sum p)
  right = mkT (\p -> right \\ subP @c @Sum p)
  sumMatch = mkT (\p -> sumMatch \\ subP @c @Sum p)

type instance SubLC c IO = (SubL c Unit, SubL c Char, SubL c List)

instance SubL c IO => IO (Term c) where
  putStrLn = mkT (\p -> putStrLn \\ subP @c @IO p)

instance SubL c IO => Monad (Term c) M.IO where
  join = mkT (\p -> join \\ subP @c @IO p)
  bind = mkT (\p -> bind \\ subP @c @IO p)

type instance SubLC c List = SubL c Y

instance SubL c List => List (Term c) where
  nil = mkT (\p -> nil \\ subP @c @List p)
  cons = mkT (\p -> cons \\ subP @c @List p)
  listMatch = mkT (\p -> listMatch \\ subP @c @List p)

type instance SubLC c Prod = SubL c DBI

instance SubL c Prod => Prod (Term c) where
  mkProd = mkT (\p -> mkProd \\ subP @c @Prod p)
  zro = mkT (\p -> zro \\ subP @c @Prod p)
  fst = mkT (\p -> fst \\ subP @c @Prod p)

instance SubL c IO => Applicative (Term c) M.IO where
  pure = mkT (\p -> pure \\ subP @c @IO p)
  ap = mkT (\p -> ap \\ subP @c @IO p)

type instance SubLC c Y = SubL c DBI

instance SubL c Y => Y (Term c) where
  y = mkT (\p -> y \\ subP @c @Y p)

instance SubL c IO => Functor (Term c) M.IO where
  map = mkT (\p -> map \\ subP @c @IO p)

type instance SubLC c Map.Map = (SubL c Prod, SubL c Option)

instance SubL c Map.Map => Map.Map (Term c) where
  lookup = mkT (\p -> Map.lookup \\ subP @c @Map.Map p)
  empty = mkT (\p -> Map.empty \\ subP @c @Map.Map p)
  singleton = mkT (\p -> Map.singleton \\ subP @c @Map.Map p)
  alter = mkT (\p -> Map.alter \\ subP @c @Map.Map p)
  mapMap = mkT (\p -> Map.mapMap \\ subP @c @Map.Map p)
  unionWith = mkT (\p -> Map.unionWith \\ subP @c @Map.Map p)

type instance SubLC c VTF.VectorTF = SubL c Double

instance SubL c VTF.VectorTF => VTF.VectorTF (Term c) where
  zero = mkT (\p -> VTF.zero \\ subP @c @VTF.VectorTF p)
  plus = mkT (\p -> VTF.plus \\ subP @c @VTF.VectorTF p)
  mult = mkT (\p -> VTF.mult \\ subP @c @VTF.VectorTF p)
  basis = mkT (\p -> VTF.basis \\ subP @c @VTF.VectorTF p)
  vtfMatch = mkT (\p -> VTF.vtfMatch \\ subP @c @VTF.VectorTF p)

type instance SubLC c Option = SubL c DBI

instance SubL c Option => Option (Term c) where
  nothing = mkT (\p -> nothing \\ subP @c @Option p)
  just = mkT (\p -> just \\ subP @c @Option p)
  optionMatch = mkT (\p -> optionMatch \\ subP @c @Option p)

mkTerm
  :: forall r l h s.
     (SubL l r)
  => (forall (repr :: * -> * -> *).
       (l repr, r repr) => repr h s)
   -> Term l h s
mkTerm f = Term k
  where k :: forall repr. l repr => repr h s
        k = f @repr \\ sub @l @r @repr

genInstance :: Q [Dec]
genInstance =
  M.mapM gen [''DBI, ''Double, ''Bool]
    where
      gen n = M.return $
        InstanceD
          M.Nothing
          []
          (AppT (AppT (ConT ''SubL) (ConT ''Lang)) (ConT n))
          [ValD (VarP 'sub) (NormalB (AppE (ConE 'Sub) (ConE 'Dict))) []]
