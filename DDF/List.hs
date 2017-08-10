{-# LANGUAGE NoMonomorphismRestriction #-}

module DDF.List (module DDF.List, module DDF.Y) where

import DDF.Y

class Y r => List r where
  nil :: r h [a]
  cons :: r h (a -> [a] -> [a])
  listMatch :: r h (b -> (a -> [a] -> b) -> [a] -> b)
  listAppend :: r h ([a] -> [a] -> [a])
  listAppend = lam2 $ \l r -> y2 (lam $ \self -> listMatch2 r (lam2 $ \a as -> cons2 a (app self as))) l

cons2 = app2 cons
listMatch2 = app2 listMatch
listAppend2 = app2 listAppend
