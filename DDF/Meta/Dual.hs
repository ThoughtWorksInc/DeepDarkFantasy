module DDF.Meta.Dual where

newtype Dual l r = Dual {runDual :: (l, r)}

dualOrig (Dual (l, _)) = l

dualDiff (Dual (_, r)) = r

mkDual l r = Dual (l, r)