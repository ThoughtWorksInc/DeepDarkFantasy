module DDF.Meta.VectorTF where

-- | F algebra of a Term Vector Spaces
data VectorTF t f = Zero | Basis t | Plus f f | Mult Double f deriving (Eq, Ord)