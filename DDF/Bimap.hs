{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  UndecidableSuperClasses,
  TypeApplications,
  ScopedTypeVariables
#-}

module DDF.Bimap (module DDF.Bimap, module DDF.Prod, module DDF.Option) where

import qualified DDF.Map as Map
import DDF.Prod
import DDF.Option
import DDF.Int
import qualified Data.Bimap as M
import qualified Prelude as M
import qualified Data.Map as M

class (Int r, Map.Map r) => Bimap r where
  size :: r h (M.Bimap a b -> M.Int)
  lookupL :: forall h a b. (Ord r a, Ord r b) => r h (M.Bimap a b -> a -> Maybe b)
  lookupL = withDict (getOrdC @r @a Proxy) $ withDict (getOrdC @r @b Proxy) lookupL'
  lookupL' :: (OrdWC r a, OrdWC r b) => r h (M.Bimap a b -> a -> Maybe b)
  lookupR :: forall h a b. (Ord r a, Ord r b) => r h (M.Bimap a b -> b -> Maybe a)
  lookupR = withDict (getOrdC @r @a Proxy) $ withDict (getOrdC @r @b Proxy) lookupR'
  lookupR' :: (OrdWC r a, OrdWC r b) => r h (M.Bimap a b -> b -> Maybe a)
  empty :: r h (M.Bimap a b)
  singleton :: r h ((a, b) -> M.Bimap a b)
  toMapL :: r h (M.Bimap a b -> M.Map a b)
  toMapR :: r h (M.Bimap a b -> M.Map b a)
  insert :: forall h a b. (Ord r a, Ord r b) => r h ((a, b) -> M.Bimap a b -> M.Bimap a b)
  insert = withDict (getOrdC @r @a Proxy) $ withDict (getOrdC @r @b Proxy) insert'
  insert' :: (OrdWC r a, OrdWC r b) => r h ((a, b) -> M.Bimap a b -> M.Bimap a b)
  updateL :: forall h a b. (Ord r a, Ord r b) => r h ((b -> Maybe b) -> a -> M.Bimap a b -> M.Bimap a b)
  updateL = withDict (getOrdC @r @a Proxy) $ withDict (getOrdC @r @b Proxy) updateL'
  updateL' :: (OrdWC r a, OrdWC r b) => r h ((b -> Maybe b) -> a -> M.Bimap a b -> M.Bimap a b)
  updateR :: forall h a b. (Ord r a, Ord r b) => r h ((a -> Maybe a) -> b -> M.Bimap a b -> M.Bimap a b)
  updateR = withDict (getOrdC @r @a Proxy) $ withDict (getOrdC @r @b Proxy) updateR'
  updateR' :: (OrdWC r a, OrdWC r b) => r h ((a -> Maybe a) -> b -> M.Bimap a b -> M.Bimap a b)

lookupL2 = app2 lookupL
size1 = app size
insert2 = app2 insert
