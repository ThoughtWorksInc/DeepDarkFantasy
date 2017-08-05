{-# LANGUAGE
  RankNTypes,
  ConstraintKinds,
  NoImplicitPrelude,
  MultiParamTypeClasses,
  KindSignatures,
  TypeOperators,
  FlexibleContexts,
  UndecidableInstances
#-}

module DDF.Term where

import DDF.Lang
import DDF.Map
import DDF.VectorTF

import qualified Prelude as M
import Data.Constraint
import ConstraintUnions

class SubL (l :: (* -> * -> *) -> Constraint) r where
  sub :: forall repr. l repr :- r repr

newtype Term c h s = Term { runTerm :: forall r. c r => r h s }

instance SubL c DBI => DBI (Term c) where

instance SubL c Bool => SubL c DBI

instance SubL c Bool => Bool (Term c) where

instance Int (Term c) where

instance Fix (Term c) where

instance Lang (Term c) where

instance FreeVector (Term c) where

instance DiffWrapper (Term c) where

instance Char (Term c) where

instance Bimap (Term c) where

instance Float (Term c) where

instance Double (Term c) where

instance Dual (Term c) where

instance Unit (Term c) where

instance Sum (Term c) where

instance IO (Term c) where

instance Monad (Term c) M.IO where

instance List (Term c) where

instance Prod (Term c) where

instance Applicative (Term c) M.IO where

instance Y (Term c) where

instance Functor (Term c) M.IO where

instance Map (Term c) where

instance VectorTF (Term c) where

instance Option (Term c) where

testing :: Term Lang () M.Double
testing = doubleZero
