{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  FlexibleContexts
#-}

module DDF.VectorTF where

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

vtfMatch4 = Double.app4 vtfMatch
vtfMatch5 = Double.app5 vtfMatch
plus2 = Double.app2 plus
mult1 = Double.app mult
mult2 = Double.app2 mult
