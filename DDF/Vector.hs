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
  mult = lam2 $ \x y -> divide2 y (recip1 x)
  default divide :: Double r => r h (v -> M.Double -> v)
  divide = lam2 $ \x y -> mult2 (recip1 y) x
  {-# MINIMAL (mult | divide) #-}

minus2 = app2 minus
mult1 = app mult
mult2 = app2 mult
divide2 = app2 divide
invert1 = app invert
minus1 = app minus
divide1 = app divide
recip = divide1 doubleOne
recip1 = app recip

instance Double r => Monoid r M.Double where
  zero = doubleZero
  plus = doublePlus

instance Double r => Group r M.Double where
  minus = doubleMinus

instance Double r => Vector r M.Double where
  type Basis M.Double = ()
  mult = doubleMult
  divide = doubleDivide
