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
  UndecidableSuperClasses
#-}

module DDF.Term where

import DDF.Lang
import DDF.Map
import DDF.VectorTF

import qualified Prelude as M
import Data.Constraint

type family SubLC (l :: (* -> * -> *) -> Constraint) (r :: (* -> * -> *) -> Constraint) :: Constraint

class SubLC l r => SubL l r where
  sub :: forall repr. l repr :- r repr

newtype Term c h s = Term { runTerm :: forall r. c r => r h s }

type instance SubLC c DBI = ()

instance SubL c DBI => DBI (Term c) where

type instance SubLC c Bool = SubL c DBI

instance SubL c Bool => Bool (Term c) where

type instance SubLC c Int = SubL c Bool

instance SubL c Int => Int (Term c) where

type instance SubLC c Fix = SubL c DBI

instance SubL c Fix => Fix (Term c) where

type instance SubLC c Lang = (SubL c Fix, SubL c Int, SubL c Float, SubL c Double, SubL c Bimap, SubL c IO, SubL c Sum, SubL c Dual)

instance SubL c Lang => Lang (Term c) where

type instance SubLC c FreeVector = SubL c DBI

instance SubLC c FreeVector => FreeVector (Term c) where

type instance SubLC c DiffWrapper = SubL c DBI

instance SubLC c DiffWrapper => DiffWrapper (Term c) where

type instance SubLC c Char = SubL c DBI

instance SubL c Char => Char (Term c) where

type instance SubLC c Bimap = (SubL c Int, SubL c Map)

instance SubL c Bimap => Bimap (Term c) where

type instance SubLC c Float = SubL c DBI

instance SubL c Float => Float (Term c) where

type instance SubLC c Double = SubL c Bool

instance SubL c Double => Double (Term c) where

type instance SubLC c Dual = SubL c Prod

instance SubL c Dual => Dual (Term c) where

type instance SubLC c Unit = SubL c DBI

instance SubL c Unit => Unit (Term c) where

type instance SubLC c Sum = SubL c DBI

instance SubL c Sum => Sum (Term c) where

type instance SubLC c IO = (SubL c Unit, SubL c Char, SubL c List)

instance SubL c IO => IO (Term c) where

instance SubL c IO => Monad (Term c) M.IO where

type instance SubLC c List = SubL c Y

instance SubLC c List => List (Term c) where

type instance SubLC c Prod = SubL c DBI

instance SubL c Prod => Prod (Term c) where

instance SubL c IO => Applicative (Term c) M.IO where

type instance SubLC c Y = SubL c DBI

instance SubL c Y => Y (Term c) where

instance SubL c IO => Functor (Term c) M.IO where

type instance SubLC c Map = (SubL c Prod, SubL c Option)

instance SubL c Map => Map (Term c) where

type instance SubLC c VectorTF = SubL c Double

instance SubLC c VectorTF => VectorTF (Term c) where

type instance SubLC c Option = SubL c DBI

instance SubL c Option => Option (Term c) where

testing :: Term Lang () M.Double
testing = doubleZero
