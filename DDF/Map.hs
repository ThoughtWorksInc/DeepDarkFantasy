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

class M.Ord x => Ord x where
  diffOrd :: Proxy (v, x) -> Dict (Ord (DiffType v x))

class (Prod r, Option r) => Map r where
  empty :: r h (M.Map k a)
  singleton :: r h (k -> a -> M.Map k a)
  lookup :: Ord k => r h (M.Map k a -> k -> Maybe a)
  alter :: Ord k => r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  mapMap :: r h ((a -> b) -> M.Map k a -> M.Map k b)