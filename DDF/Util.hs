{-# LANGUAGE TupleSections #-}

module DDF.Util where

import System.Random
import GHC.Float

vars = [pre : suf | suf <- "":map show [0..], pre <- ['a'..'z']]

isSquare n = sq * sq == n
  where sq = floor $ sqrt (fromIntegral n::Double)

instance Random () where
  random = ((),)
  randomR _ = random

instance (Random l, Random r) => Random (l, r) where
  random g0 = ((l, r), g2)
    where
      (l, g1) = random g0
      (r, g2) = random g1
  randomR ((llo, rlo), (lhi, rhi)) g0 = ((l, r), g2)
    where
      (l, g1) = randomR (llo, lhi) g0
      (r, g2) = randomR (rlo, rhi) g1

class RandRange w where
  randRange :: (Double, Double) -> (w, w)

instance RandRange () where
  randRange _ = ((), ())

instance RandRange Double where
  randRange (lo, hi) = (lo, hi)

instance RandRange Float where
  randRange (lo, hi) = (double2Float lo, double2Float hi)

instance (RandRange l, RandRange r) => RandRange (l, r) where
  randRange (lo, hi) = ((llo, rlo), (lhi, rhi))
    where
      (llo, lhi) = randRange (lo, hi)
      (rlo, rhi) = randRange (lo, hi)
