{-# LANGUAGE
  NoImplicitPrelude,
  DefaultSignatures,
  MultiParamTypeClasses,
  NoMonomorphismRestriction,
  FlexibleContexts,
  FlexibleInstances,
  TypeFamilies
#-}

module DDF.Vector where
import DDF.Double
import qualified Prelude as M
import DDF.Meta.FreeVector

class Monoid r g => Group r g where
  invert :: r h (g -> g)
  minus :: r h (g -> g -> g)
  default invert :: DBI r => r h (g -> g)
  invert = minus1 zero
  default minus :: DBI r => r h (g -> g -> g)
  minus = lam2 $ \x y -> plus2 x (invert1 y)
  {-# MINIMAL (invert | minus) #-}

class Group r v => Vector r v where
  type Basis v :: *
  mult :: r h (M.Double -> v -> v)
  divide :: r h (v -> M.Double -> v)
  default mult :: Double r => r h (M.Double -> v -> v)
  mult = lam2 $ \x y -> divide2 y (app2 doubleDivide doubleOne x)
  default divide :: Double r => r h (v -> M.Double -> v)
  divide = lam2 $ \x y -> mult2 (app2 doubleDivide doubleOne y) x
  toFreeVector :: r h (v -> FreeVector (Basis v) M.Double)
  {-# MINIMAL (mult | divide), toFreeVector #-}

minus2 = app2 minus
mult1 = app mult
mult2 = app2 mult
divide2 = app2 divide
invert1 = app invert
minus1 = app minus
divide1 = app divide
recip = divide1 doubleOne
recip1 = app recip
toFreeVector1 = app toFreeVector