{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction #-}

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
  lookupL :: (Map.Ord r a, Map.Ord r b) => r h (M.Bimap a b -> a -> Maybe b)
  lookupR :: (Map.Ord r a, Map.Ord r b) => r h (M.Bimap a b -> b -> Maybe a)
  empty :: r h (M.Bimap a b)
  singleton :: r h ((a, b) -> M.Bimap a b)
  toMapL :: r h (M.Bimap a b -> M.Map a b)
  toMapR :: r h (M.Bimap a b -> M.Map b a)
  insert :: (Map.Ord r a, Map.Ord r b) => r h ((a, b) -> M.Bimap a b -> M.Bimap a b) 
  updateL :: (Map.Ord r a, Map.Ord r b) => r h ((b -> Maybe b) -> a -> M.Bimap a b -> M.Bimap a b)
  updateR :: (Map.Ord r a, Map.Ord r b) => r h ((a -> Maybe a) -> b -> M.Bimap a b -> M.Bimap a b)

lookupL2 = app2 lookupL
size1 = app size
insert2 = app2 insert