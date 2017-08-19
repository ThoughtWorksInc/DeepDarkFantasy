{-# LANGUAGE
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies,
  KindSignatures,
  MultiParamTypeClasses,
  FlexibleInstances,
  NoMonomorphismRestriction,
  ConstraintKinds,
  DataKinds,
  FlexibleContexts
#-}

module DDF.Meta.Diff (module DDF.Meta.Diff, module DDF.Vector) where

import DDF.Vector
import DDF.ImportMeta

type family DiffType (v :: *) (x :: *)

newtype Diff r v h x = Diff {runDiff :: r (DiffType v h) (DiffType v x)}

class Ord x => MetaOrd x where
  diffOrd :: Proxy (v, x) -> Dict (MetaOrd (DiffType v x))
