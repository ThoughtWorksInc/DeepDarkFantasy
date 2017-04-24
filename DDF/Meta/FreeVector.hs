module DDF.Meta.FreeVector where

newtype FreeVector b d = FreeVector {runFreeVector :: b -> d}