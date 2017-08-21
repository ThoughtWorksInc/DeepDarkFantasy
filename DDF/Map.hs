{-# LANGUAGE
  NoImplicitPrelude,
  ScopedTypeVariables,
  TypeApplications,
  FlexibleInstances,
  NoMonomorphismRestriction,
  MultiParamTypeClasses
#-}

module DDF.Map (module DDF.Map, module DDF.Prod, module DDF.Option, module DDF.Ordering) where

import DDF.Prod
import DDF.Option
import DDF.Ordering
import qualified Data.Map as M

class (Prod r, Option r) => Map r where
  empty :: r h (M.Map k a)
  singleton :: r h (k -> a -> M.Map k a)
  lookup :: Ord r k => r h (M.Map k a -> k -> Maybe a)
  alter :: Ord r k => r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  mapMap :: r h ((a -> b) -> M.Map k a -> M.Map k b)
  unionWith :: Ord r k => r h ((a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
  insert :: Ord r k => r h (k -> a -> M.Map k a -> M.Map k a)
  insert = lam2 $ \k a -> alter2 (const1 (just1 a)) k

lookup2 = app2 lookup
unionWith1 = app unionWith
mapMap1 = app mapMap
mapMap2 = app2 mapMap
insert2 = app2 insert
alter2 = app2 alter
singleton2 = app2 singleton
