{-# LANGUAGE
  NoImplicitPrelude,
  ScopedTypeVariables,
  TypeApplications,
  FlexibleInstances
#-}

module DDF.Map (module DDF.Map, module DDF.Prod, module DDF.Option) where

import DDF.Prod
import qualified Prelude as M
import qualified Data.Map as M
import DDF.Option
import DDF.Meta.Diff
import qualified DDF.Meta.Dual as M

class M.Ord x => Ord x where
  diffOrd :: Proxy (v, x) -> Dict (Ord (DiffType v x))
  liftDiffOrd :: Proxy x -> Dict (Ord (LiftDiff x))

instance Ord () where
  diffOrd _ = Dict
  liftDiffOrd _ = Dict

instance Ord a => Ord [a] where
  diffOrd (_ :: Proxy (v, [a])) = withDict (diffOrd (Proxy :: Proxy (v, a))) Dict
  liftDiffOrd _ = withDict (liftDiffOrd @a Proxy) Dict

instance M.Eq l => M.Eq (M.Dual l r) where
  M.Dual (l, _) == M.Dual (r, _) = l == r

instance M.Ord l => M.Ord (M.Dual l r) where
  M.Dual (l, _) `compare` M.Dual (r, _) = l `compare` r

instance Ord l => Ord (M.Dual l r) where
  diffOrd (_ :: Proxy (v, M.Dual l r)) = withDict (diffOrd (Proxy :: Proxy (v, l))) Dict
  liftDiffOrd _ = withDict (liftDiffOrd @l Proxy) Dict

instance M.Eq x => M.Eq (InfDiff Eval () x) where
  InfDiff (Combine (Eval r) _) == InfDiff (Combine (Eval l) _) = l () == r ()

instance M.Ord x => M.Ord (InfDiff Eval () x) where
  InfDiff (Combine (Eval r) _) `compare` InfDiff (Combine (Eval l) _) = l () `compare` r ()

instance Ord x => Ord (InfDiff Eval () x) where
  diffOrd (_ :: Proxy (v, InfDiff Eval () x)) = diffOrd @x @v Proxy `withDict` Dict
  liftDiffOrd _ = liftDiffOrd @x Proxy `withDict` Dict

instance Ord M.Double where
  diffOrd _ = Dict
  liftDiffOrd _ = Dict

instance Ord M.Float where
  diffOrd _ = Dict
  liftDiffOrd _ = Dict

class (Prod r, Option r) => Map r where
  empty :: r h (M.Map k a)
  singleton :: r h (k -> a -> M.Map k a)
  lookup :: Ord k => r h (k -> M.Map k a -> Maybe a)
  alter :: Ord k => r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  mapMap :: r h ((a -> b) -> M.Map k a -> M.Map k b)