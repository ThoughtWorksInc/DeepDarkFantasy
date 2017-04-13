module DDF.Meta.InfDiff where

import DDF.Meta.Combine
import DDF.Meta.GWDiff

newtype InfDiff r h x = InfDiff {runInfDiff :: Combine r (GWDiff (InfDiff r)) h x}
