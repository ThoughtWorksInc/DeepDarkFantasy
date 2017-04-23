module DDF.Meta.Dual where

newtype Dual l r = Dual {runDual :: (l, r)}

instance Eq l => Eq (Dual l r) where
  (Dual (l, _)) == (Dual (r, _)) = l == r

instance Ord l => Ord (Dual l r) where
  (Dual (l, _)) `compare` (Dual (r, _)) = l `compare` r

dualOrig (Dual (l, _)) = l

dualDiff (Dual (_, r)) = r

mkDual l r = Dual (l, r)