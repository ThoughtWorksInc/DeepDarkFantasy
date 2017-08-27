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
  lookup :: forall h k a. Ord r k => r h (M.Map k a -> k -> Maybe a)
  lookup = withDict (getOrdC @r @k Proxy) lookup'
  lookup' :: OrdWC r k => r h (M.Map k a -> k -> Maybe a)
  alter :: forall h k a. Ord r k => r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  alter = withDict (getOrdC @r @k Proxy) alter'
  alter' :: OrdWC r k => r h ((Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a)
  mapMap :: r h ((a -> b) -> M.Map k a -> M.Map k b)
  unionWithKey :: forall h k a. Ord r k => r h ((k -> a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
  unionWithKey = withDict (getOrdC @r @k Proxy) unionWithKey'
  unionWithKey' :: OrdWC r k => r h ((k -> a -> a -> a) -> M.Map k a -> M.Map k a -> M.Map k a)
  insert :: forall h k a. Ord r k => r h (k -> a -> M.Map k a -> M.Map k a)
  insert = withDict (getOrdC @r @k Proxy) insert'
  insert' :: OrdWC r k => r h (k -> a -> M.Map k a -> M.Map k a)
  insert' = lam2 $ \k a -> alter2 (const1 $ just1 a) k
  delete :: forall h k a. Ord r k => r h (k -> M.Map k a -> M.Map k a)
  delete = withDict (getOrdC @r @k Proxy) delete'
  delete' :: OrdWC r k => r h (k -> M.Map k a -> M.Map k a)
  delete' = lam $ \k -> alter2 (const1 nothing) k
  -- | While singleton doesnt need the ord instance in Data.Map,
  -- doing so will improve difficulty of writing all sort of interpreter,
  -- and I dont know any use of singleton (without Ord).
  singleton :: forall h k a. Ord r k => r h (k -> a -> M.Map k a)
  singleton = withDict (getOrdC @r @k Proxy) singleton'
  singleton' :: OrdWC r k => r h (k -> a -> M.Map k a)
  singleton' = lam2 $ \k a -> insert3 k a empty

lookup2 = app2 lookup
unionWithKey1 = app1 unionWithKey
unionWithKey2 = app2 unionWithKey
unionWithKey3 = app3 unionWithKey
mapMap1 = app1 mapMap
mapMap2 = app2 mapMap
insert1 = app1 insert
insert2 = app2 insert
insert3 = app3 insert
alter1 = app1 alter
alter2 = app2 alter
alter3 = app3 alter
delete1 = app1 delete
delete2 = app2 delete
singleton1 = app1 singleton
singleton2 = app2 singleton