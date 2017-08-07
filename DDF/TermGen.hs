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
import DDF.Map
import DDF.VectorTF

import qualified Prelude as M
import Data.Constraint
import Language.Haskell.TH
import Data.Proxy

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

type instance SubLC c Bool = SubL c DBI

instance SubL c Bool => Bool (Term c) where
  bool x = mkT (\p -> bool x \\ subP @c @Bool p)

type instance SubLC c Int = SubL c Bool

instance SubL c Int => Int (Term c) where
  int x = mkT (\p -> int x \\ subP @c @Int p)

type instance SubLC c Fix = SubL c DBI

instance SubL c Fix => Fix (Term c) where
  fix = mkT (\p -> fix \\ subP @c @Fix p)
  runFix = mkT (\p -> runFix \\ subP @c @Fix p)

type instance SubLC c Lang = (SubL c Fix, SubL c Int, SubL c Float, SubL c Double, SubL c Bimap, SubL c IO, SubL c Sum, SubL c Dual, SubL c DiffWrapper)

instance SubL c Lang => Lang (Term c) where

type instance SubLC c FreeVector = SubL c DBI

instance SubLC c FreeVector => FreeVector (Term c) where

type instance SubLC c DiffWrapper = SubL c DBI

instance SubL c DiffWrapper => DiffWrapper (Term c) where
  diffWrapper = mkT (\p -> diffWrapper \\ subP @c @DiffWrapper p)
  runDiffWrapper = mkT (\p -> runDiffWrapper \\ subP @c @DiffWrapper p)

type instance SubLC c Char = SubL c DBI

instance SubL c Char => Char (Term c) where
  char x = mkT (\p -> char x \\ subP @c @Char p)

type instance SubLC c Bimap = (SubL c Int, SubL c Map)

instance SubL c Bimap => Bimap (Term c) where

type instance SubLC c Float = SubL c DBI

instance SubL c Float => Float (Term c) where

type instance SubLC c Double = SubL c Bool

instance SubL c Double => Double (Term c) where

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

type instance SubLC c List = SubL c Y

instance SubL c List => List (Term c) where
  nil = mkT (\p -> nil \\ subP @c @List p)
  cons = mkT (\p -> cons \\ subP @c @List p)
  listMatch = mkT (\p -> listMatch \\ subP @c @List p)

type instance SubLC c Prod = SubL c DBI

instance SubL c Prod => Prod (Term c) where

instance SubL c IO => Applicative (Term c) M.IO where
  pure = mkT (\p -> pure \\ subP @c @IO p)
  ap = mkT (\p -> ap \\ subP @c @IO p)

type instance SubLC c Y = SubL c DBI

instance SubL c Y => Y (Term c) where
  y = mkT (\p -> y \\ subP @c @Y p)

instance SubL c IO => Functor (Term c) M.IO where
  map = mkT (\p -> map \\ subP @c @IO p)

type instance SubLC c Map = (SubL c Prod, SubL c Option)

instance SubL c Map => Map (Term c) where

type instance SubLC c VectorTF = SubL c Double

instance SubLC c VectorTF => VectorTF (Term c) where

type instance SubLC c Option = SubL c DBI

instance SubL c Option => Option (Term c) where
  nothing = mkT (\p -> nothing \\ subP @c @Option p)
  just = mkT (\p -> just \\ subP @c @Option p)
  optionMatch = mkT (\p -> optionMatch \\ subP @c @Option p)

genInstance :: Q [Dec]
genInstance =
  M.mapM gen [''DBI, ''Double, ''Bool]
    where
      gen n = M.return $
        InstanceD
          M.Nothing
          []
          (AppT (AppT (ConT ''SubL) (ConT ''Lang)) (ConT n))
          []
