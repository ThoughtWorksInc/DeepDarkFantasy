{-# LANGUAGE RankNTypes #-}

module DDF.Meta.GWDiff where

import DDF.ImportMeta
import DDF.Diff
import DDF.Vector

newtype GWDiff r h x = GWDiff {runGWDiff :: forall v. Vector r v => Proxy v -> r (Diff v h) (Diff v x)}
