module DDF.Meta.Combine where

data Combine l r h x = Combine (l h x) (r h x)
