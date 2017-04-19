{-# LANGUAGE
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies,
  KindSignatures,
  MultiParamTypeClasses,
  FlexibleInstances,
  NoMonomorphismRestriction,
  ConstraintKinds,
  DataKinds,
  FlexibleContexts
#-}

module DDF.Meta.Diff (module DDF.Meta.Diff, module DDF.Meta.Interpreter, module DDF.Vector) where

import DDF.Vector
import DDF.Meta.Interpreter
import DDF.Meta.Dual
import Data.Map

type family DiffType (v :: *) (x :: *)
type instance DiffType v () = ()
type instance DiffType v (l, r) = (DiffType v l, DiffType v r)
type instance DiffType v (l -> r) = DiffType v l -> DiffType v r
type instance DiffType v Void = Void
type instance DiffType v Double = Dual Double v
type instance DiffType v Float = Dual Float v
type instance DiffType v (Writer l r) = Writer (DiffType v l) (DiffType v r)
type instance DiffType v (IO l) = IO (DiffType v l)
type instance DiffType v (Maybe l) = Maybe (DiffType v l)
type instance DiffType v [l] = [DiffType v l]
type instance DiffType v (Either l r) = Either (DiffType v l) (DiffType v r)
type instance DiffType v (State l r) = State (DiffType v l) (DiffType v r)
type instance DiffType v Bool = Bool
type instance DiffType v Char = Char
type instance DiffType v (Map k val) = Map (DiffType v k) (DiffType v val)
type instance DiffType v (Dual l r) = Dual (DiffType v l) (DiffType v r)
type instance DiffType v (InfDiff Eval () x) = InfDiff Eval () x

newtype GDiff r h x = GDiff {runGDiff :: forall v. (Vector (InfDiff Eval) v, Vector r v, DiffVector r v, RTDiff r v) =>
  Proxy v -> r (DiffType v h) (DiffType v x)}

newtype InfDiff r h x = InfDiff {runInfDiff :: Combine r (GDiff (InfDiff r)) h x}

newtype Diff r v h x = Diff {runDiff :: r (DiffType v h) (DiffType v x)}

type family DiffInt (r :: * -> * -> *) :: * -> * -> *

type family DiffVector (r :: * -> * -> *) v :: Constraint

type family RTDiff (r :: * -> * -> *) x :: Constraint

type family LiftDiff (x :: *)
type instance LiftDiff () = ()
type instance LiftDiff (l, r) = (LiftDiff l, LiftDiff r)
type instance LiftDiff (l -> r) = LiftDiff l -> LiftDiff r
type instance LiftDiff Void = Void
type instance LiftDiff Double = InfDiff Eval () Double
type instance LiftDiff Float = InfDiff Eval () Float
type instance LiftDiff (Writer l r) = Writer (LiftDiff l) (LiftDiff r)
type instance LiftDiff (IO l) = IO (LiftDiff l)
type instance LiftDiff (Maybe l) = Maybe (LiftDiff l)
type instance LiftDiff [l] = [LiftDiff l]
type instance LiftDiff (Either l r) = Either (LiftDiff l) (LiftDiff r)
type instance LiftDiff (State l r) = State (LiftDiff l) (LiftDiff r)
type instance LiftDiff Bool = Bool
type instance LiftDiff Char = Char
type instance LiftDiff (Map k val) = Map (LiftDiff k) (LiftDiff val)
type instance LiftDiff (Dual l r) = Dual (LiftDiff l) (LiftDiff r)
type instance LiftDiff (InfDiff Eval () x) = LiftDiff x