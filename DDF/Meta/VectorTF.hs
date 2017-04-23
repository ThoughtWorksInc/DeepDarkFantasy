module DDF.Meta.VectorTF where

-- | F algebra of a Term Vector Spaces
data VectorTF t f = ZeroVTF | BasisVTF t | PlusVTF f f | MultVTF Double f