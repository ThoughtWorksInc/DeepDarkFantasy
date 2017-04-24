{-# LANGUAGE KindSignatures, DataKinds, TypeFamilies, TypeOperators, UndecidableInstances #-}

module DDF.Meta.DiffWrapper (module DDF.Meta.DiffWrapper, module DDF.Meta.Diff) where

import DDF.Meta.Diff

type family FDiffType (a :: [*]) x
type instance FDiffType '[] x = x
type instance FDiffType (a ': as) x = DiffType a (FDiffType as x)

newtype DiffWrapper (a :: [*]) x = DiffWrapper {runDiffWrapper :: FDiffType a x}