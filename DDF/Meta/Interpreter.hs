{-# LANGUAGE
  RankNTypes,
  TypeApplications,
  TypeOperators,
  ScopedTypeVariables,
  MultiParamTypeClasses,
  ExistentialQuantification,
  FlexibleInstances,
  InstanceSigs
#-}

module DDF.Meta.Interpreter (module DDF.Meta.Interpreter, module DDF.ImportMeta) where

import DDF.ImportMeta

newtype Eval h x = Eval {runEval :: h -> x}

newtype UnHOAS repr h x = UnHOAS {runUnHOAS :: repr h x}

class ProdCon con l r where
  prodCon :: (con l, con r) :- con (l, r)

instance ProdCon Random l r where prodCon = Sub Dict

instance ProdCon RandRange l r where prodCon = Sub Dict

instance ProdCon Show l r where prodCon = Sub Dict

class Weight w where
  weightCon :: (con (), con Float, con Double, ForallV (ProdCon con)) :- con w

instance Weight () where weightCon = Sub Dict

instance Weight Double where weightCon = Sub Dict

instance Weight Float where weightCon = Sub Dict

instance (Weight l, Weight r) => Weight (l, r) where
  weightCon :: forall con. (con (), con Float, con Double, ForallV (ProdCon con)) :- con (l, r)
  weightCon = Sub (mapDict (prodCon \\ (instV :: (ForallV (ProdCon con) :- ProdCon con l r))) (Dict \\ weightCon @l @con \\ weightCon @r @con))

data RunImpW repr h x = forall w. Weight w => RunImpW (repr h (w -> x))
data ImpW repr h x = NoImpW (repr h x) | forall w. Weight w => ImpW (repr h (w -> x))
type RunImpWR repr h x = forall r. (forall w. Weight w => repr h (w -> x) -> r) -> r

runImpW2RunImpWR :: RunImpW repr h x -> RunImpWR repr h x
runImpW2RunImpWR (RunImpW x) = \f -> f x

runImpWR2RunImpW :: RunImpWR repr h x -> RunImpW repr h x
runImpWR2RunImpW f = f RunImpW
