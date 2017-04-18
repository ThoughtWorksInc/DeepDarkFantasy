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

module DDF.Diff (module DDF.Diff, module DDF.Meta.Interpreter, module DDF.Vector) where

import DDF.Vector
import DDF.Meta.Interpreter
import DDF.Meta.Dual
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
type instance Diff v (Map k val) = Map (Diff v k) (Diff v val)
type instance Diff v (Dual l r) = Dual (Diff v l) (Diff v r)
type instance Diff v (InfDiff Eval () x) = InfDiff Eval () x

newtype GWDiff r h x = GWDiff {runGWDiff :: forall v. (Vector (InfDiff Eval) v, Vector r v, DiffVector r v, RTDiff r v) =>
  Proxy v -> r (Diff v h) (Diff v x)}

newtype InfDiff r h x = InfDiff {runInfDiff :: Combine r (GWDiff (InfDiff r)) h x}

newtype WDiff r v h x = WDiff {runWDiff :: r (Diff v h) (Diff v x)}

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