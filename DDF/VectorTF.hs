{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleContexts,
  UndecidableSuperClasses,
  UndecidableInstances,
  FlexibleInstances,
  MultiParamTypeClasses,
  TypeOperators,
  TypeApplications,
  ScopedTypeVariables
#-}

module DDF.VectorTF where

import DDF.ImportMeta
import qualified DDF.Double as Double
import qualified DDF.Meta.VectorTF as M
import qualified Prelude as M
import qualified DDF.Ordering as Ord

class Double.Double r => VectorTF r where
  zero :: r h (M.VectorTF t f)
  basis :: r h (t -> M.VectorTF t f)
  plus :: r h (f -> f -> M.VectorTF t f)
  mult :: r h (M.Double -> f -> M.VectorTF t f)
  vtfMatch :: r h (a -> (t -> a) -> (f -> f -> a) -> (M.Double -> f -> a) -> M.VectorTF t f -> a)
  vtfCmp :: r h (Ord.Cmp t -> Ord.Cmp f -> Ord.Cmp (M.VectorTF t f))
  vtfGetOrdC :: (Ord.Ord r t, Ord.Ord r f) :- (Ord.OrdC r (M.VectorTF t f))

instance (Ord.Ord r t, Ord.Ord r f, VectorTF r) => Ord.Ord r (M.VectorTF t f) where
  cmp = Double.app2 vtfCmp Ord.cmp Ord.cmp
  getOrdC _ = Ord.Dict \\ vtfGetOrdC @r @t @f

vtfMatch4 = Double.app4 vtfMatch
vtfMatch5 = Double.app5 vtfMatch
plus2 = Double.app2 plus
mult1 = Double.app mult
mult2 = Double.app2 mult
