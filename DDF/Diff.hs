{-# LANGUAGE
  NoImplicitPrelude,
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies,
  KindSignatures,
  MultiParamTypeClasses,
  FlexibleInstances,
  NoMonomorphismRestriction
#-}

module DDF.Diff where

import DDF.ImportMeta
import DDF.Meta.Dual
import Prelude
import Data.Map

type family Diff (v :: *) (x :: *)
type instance Diff v () = ()
type instance Diff v (l, r) = (Diff v l, Diff v r)
type instance Diff v (l -> r) = Diff v l -> Diff v r
type instance Diff v Void = Void
type instance Diff v Double = Dual Double v
type instance Diff v Float = Dual Float v
type instance Diff v (Writer l r) = Writer (Diff v l) (Diff v r)
type instance Diff v (IO l) = IO (Diff v l)
type instance Diff v (Maybe l) = Maybe (Diff v l)
type instance Diff v [l] = [Diff v l]
type instance Diff v (Either l r) = Either (Diff v l) (Diff v r)
type instance Diff v (State l r) = State (Diff v l) (Diff v r)
type instance Diff v Bool = Bool
type instance Diff v Char = Char
type instance Diff v Ordering = Ordering
type instance Diff v (Map k val) = Map (Diff v k) (Diff v val)
type instance Diff v (Dual l r) = Dual (Diff v l) (Diff v r)