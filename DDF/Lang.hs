{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  MultiParamTypeClasses,
  FlexibleInstances
#-}

module DDF.Lang (
  module DDF.Lang,
  module DDF.Bool,
  module DDF.Char,
  module DDF.Double,
  module DDF.Float,
  module DDF.Bimap,
  module DDF.Dual,
  module DDF.Meta.Diff,
  module DDF.Unit,
  module DDF.Sum,
  module DDF.Int,
  module DDF.IO,
  module DDF.DiffWrapper,
  module DDF.Fix
) where

import DDF.Bool
import DDF.Char
import DDF.Double
import DDF.Float
import DDF.Bimap
import DDF.Dual
import DDF.Vector
import DDF.Meta.Diff
import DDF.Unit
import DDF.Sum
import DDF.Int
import DDF.IO
import DDF.DiffWrapper
import DDF.Fix

import qualified DDF.VectorTF as VTF
import qualified DDF.Meta.Dual as M
import qualified Control.Monad.Writer as M (Writer)
import qualified GHC.Float as M
import qualified Prelude as M
import qualified Data.Map as M
import qualified DDF.Map as Map
import qualified Data.Map as M.Map

type FreeVector b = b -> M.Double
type FreeVectorBuilder b = M.Map.Map b M.Double

class (Bool r, Char r, Double r, Float r, Bimap r, Dual r, Unit r, Sum r, Int r, IO r, VTF.VectorTF r, DiffWrapper r, Fix r) => Lang r where
  exfalso :: r h (Void -> a)
  writer :: r h ((a, w) -> M.Writer w a)
  runWriter :: r h (M.Writer w a -> (a, w))
  float2Double :: r h (M.Float -> M.Double)
  double2Float :: r h (M.Double -> M.Float)
  state :: r h ((x -> (y, x)) -> State x y)
  runState :: r h (State x y -> (x -> (y, x)))
  iterate :: r h ((x -> x) -> x -> [x])
  iterate = lam $ \f -> y1 $ lam2 $ \fi x -> cons2 x (app fi (app f x))
  buildFreeVector :: Map.Ord b => r h (FreeVectorBuilder b -> FreeVector b)
  buildFreeVector = lam2 $ \fb b -> optionMatch3 (double 0) id (Map.lookup2 fb b)

class Reify r x where
  reify :: x -> r h x

instance Lang r => Reify r () where
  reify _ = unit

instance Lang r => Reify r M.Double where
  reify = double

instance (Lang repr, Reify repr l, Reify repr r) => Reify repr (l, r) where
  reify (l, r) = mkProd2 (reify l) (reify r)

instance Lang repr => ProdCon (Monoid repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Reify repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Vector repr) l r where prodCon = Sub Dict

instance Lang r => Monoid r () where
  zero = unit
  plus = const1 $ const1 unit

instance Lang r => Group r () where
  invert = const1 unit
  minus = const1 $ const1 unit

instance Lang r => Vector r () where
  mult = const1 $ const1 unit
  divide = const1 $ const1 unit

instance Float r => Monoid r M.Float where
  zero = floatZero
  plus = floatPlus

instance Float r => Group r M.Float where
  minus = floatMinus

instance Lang r => Vector r M.Float where
  mult = com2 floatMult double2Float
  divide = com2 (flip2 com double2Float) floatDivide

instance (Prod repr, Monoid repr l, Monoid repr r) => Monoid repr (l, r) where
  zero = mkProd2 zero zero
  plus = lam2 $ \l r -> mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))

instance (Prod repr, Group repr l, Group repr r) => Group repr (l, r) where
  invert = bimap2 invert invert

instance (Prod repr, Double repr, Vector repr l, Vector repr r) => Vector repr (l, r) where
  mult = lam $ \x -> bimap2 (mult1 x) (mult1 x)

instance (Double r, Monoid r v) => Monoid r (M.Double -> v) where
  zero = const1 zero
  plus = lam3 $ \l r x -> plus2 (app l x) (app r x)

instance (Lang r, Group r v) => Group r (M.Double -> v) where
  invert = lam2 $ \l x -> app l (invert1 x)

instance (Lang r, Vector r v) => Vector r (M.Double -> v) where
  mult = lam3 $ \l r x -> app r (mult2 l x)

instance Lang r => Monoid r [a] where
  zero = nil
  plus = listAppend

instance {-# INCOHERENT #-} Lang r => Functor r [] where
  map = lam $ \f -> y1 $ lam $ \self -> listMatch2 nil (lam2 $ \x xs -> cons2 (app f x) $ app self xs)

instance Lang r => BiFunctor r M.Either where
  bimap = lam2 $ \l r -> sumMatch2 (com2 left l) (com2 right r)

instance Prod r => BiFunctor r (,) where
  bimap = lam3 $ \l r p -> mkProd2 (app l (zro1 p)) (app r (fst1 p))

instance Dual r => BiFunctor r M.Dual where
  bimap = lam2 $ \l r -> dual `com2` bimap2 l r `com2` runDual

instance Lang r => Functor r (Writer w) where
  map = lam $ \f -> com2 writer (com2 (bimap2 f id) runWriter)

instance Lang r => Functor r (M.Map k) where
  map = Map.mapMap

instance (Lang r, Monoid r w) => Applicative r (Writer w) where
  pure = com2 writer (flip2 mkProd zero)
  ap = lam2 $ \f x -> writer1 (mkProd2 (app (zro1 (runWriter1 f)) (zro1 (runWriter1 x))) (plus2 (fst1 (runWriter1 f)) (fst1 (runWriter1 x))))

instance (Lang r, Monoid r w) => Monad r (Writer w) where
  join = lam $ \x -> writer1 $ mkProd2 (zro1 $ runWriter1 $ zro1 $ runWriter1 x) (plus2 (fst1 $ runWriter1 $ zro1 $ runWriter1 x) (fst1 $ runWriter1 x))

instance Lang r => Functor r (State l) where
  map = lam2 $ \f st -> state1 (com2 (bimap2 f id) (runState1 st))

instance Lang r => Applicative r (State l) where
  pure = lam $ \x -> state1 (mkProd1 x)
  ap = lam2 $ \f x -> state1 $ lam $ \st -> let_2 (runState2 f st) (lam $ \p -> bimap3 (zro1 p) id (runState2 x (fst1 p)))

instance Lang r => Monad r (State l) where
  join = lam $ \x -> state1 $ lam $ \st -> let_2 (runState2 x st) (uncurry1 runState)

instance Lang r => Functor r M.Maybe where
  map = lam $ \func -> optionMatch2 nothing (com2 just func)

instance Lang r => Applicative r M.Maybe where
  pure = just
  ap = optionMatch2 (const1 nothing) map

instance Lang r => Monad r M.Maybe where
  bind = lam2 $ \x func -> optionMatch3 nothing func x

instance Lang r => Monoid r (FreeVector b) where
  zero = const1 (double 0)
  plus = lam3 $ \l r x -> app l x `plus2` app r x

instance Lang r => Group r (FreeVector b) where
  invert = lam2 $ \f x -> invert1 (app f x)
  minus = lam3 $ \l r x -> app l x `minus2` app r x

instance Lang r => Vector r (FreeVector b) where
  mult = lam3 $ \d l x -> d `mult2` app l x
  divide = lam3 $ \l d x -> app l x `divide2` d

uncurry1 = app uncurry
optionMatch2 = app2 optionMatch
optionMatch3 = app3 optionMatch
writer1 = app writer
runWriter1 = app runWriter
float2Double1 = app float2Double
doubleExp1 = app doubleExp
floatExp1 = app floatExp
state1 = app state
runState1 = app runState
runState2 = app2 runState