{-# LANGUAGE
  NoImplicitPrelude,
  NoMonomorphismRestriction,
  MultiParamTypeClasses,
  FlexibleInstances,
  TypeFamilies,
  ScopedTypeVariables,
  FlexibleContexts,
  UndecidableInstances,
  TypeApplications,
  PartialTypeSignatures,
  UndecidableSuperClasses
#-}

module DDF.Lang (
  module DDF.Lang,
  module DDF.Bimap,
  module DDF.Bool,
  module DDF.Char,
  module DDF.Double,
  module DDF.Dual,
  module DDF.Float,
  module DDF.Meta.Diff,
  module DDF.Ordering,
  module DDF.Unit,
  module DDF.Sum,
  module DDF.Int,
  module DDF.IO,
  module DDF.DiffWrapper,
  module DDF.Fix,
  module DDF.FreeVector
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
import DDF.FreeVector
import DDF.Ordering

import qualified DDF.VectorTF as VTF
import qualified DDF.Meta.VectorTF as M.VTF
import qualified DDF.Meta.Dual as M
import qualified Control.Monad.Writer as M (Writer)
import qualified GHC.Float as M
import qualified Prelude as M
import qualified Data.Map as M
import qualified DDF.Map as Map
import qualified Data.Map as M.Map
import qualified Data.Functor.Foldable as M
import qualified Data.Bimap as M.Bimap
import qualified DDF.Meta.FreeVector as M

type FreeVectorBuilder b = M.Map.Map b M.Double
type SVTFBuilder b = State (M.Bimap.Bimap (M.VTF.VectorTF b M.Int) M.Int) M.Int
class (Ordering r, Char r, Double r, Float r, Bimap r, Dual r, Unit r, Sum r, Int r, IO r, VTF.VectorTF r, DiffWrapper r, Fix r, FreeVector r) => Lang r where
  exfalso :: r h (Void -> a)
  writer :: r h ((a, w) -> M.Writer w a)
  runWriter :: r h (M.Writer w a -> (a, w))
  float2Double :: r h (M.Float -> M.Double)
  double2Float :: r h (M.Double -> M.Float)
  state :: r h ((x -> (y, x)) -> State x y)
  runState :: r h (State x y -> (x -> (y, x)))
  iterate :: r h ((x -> x) -> x -> [x])
  iterate = lam $ \f -> y1 $ lam2 $ \fi x -> cons2 x (app fi (app f x))
  buildFreeVector :: MetaOrd b => r h (FreeVectorBuilder b -> M.FreeVector b M.Double)
  buildFreeVector = lam $ \fb -> freeVector1 $ lam $ \b -> optionMatch3 (double 0) id (Map.lookup2 fb b)
  toSVTFBuilder :: forall h b. MetaOrd b => r h (M.VTF.VectorTF b M.Int -> SVTFBuilder b)
  toSVTFBuilder =
    lam $ \x -> state1 $ lam $ \m ->
      optionMatch3
        (let_2 (size1 m) (lam $ \si -> mkProd2 si (insert2 (mkProd2 x si) m)))
        (lam $ \xid -> mkProd2 xid m)
        (lookupL2 m x)
  get :: r h (Maybe a -> a)
  get = optionMatch2 undefined id
  getVar :: r h (State x x)
  getVar = state1 (dup1 mkProd)
  update :: r h ((x -> x) -> State x ())
  update = lam $ \f -> state1 $ lam $ \x -> mkProd2 unit (app f x)
  updateWengert :: r h (M.Int -> M.Double -> M.Map.Map M.Int M.Double -> M.Map M.Int M.Double)
  updateWengert = lam2 $ \i d -> Map.alter2 (optionMatch2 (just1 d) (just `com2` (plus1 d))) i
  vtfCata :: r h ((M.VTF.VectorTF a b -> b) -> M.Fix (M.VTF.VectorTF a) -> b)
  vtfCata = lam $ \f -> y1 $ lam $ \fx ->
    VTF.vtfMatch4
      (app f VTF.zero)
      (f `com2` VTF.basis)
      (lam2 $ \l r -> app f (VTF.plus2 (app fx l) (app fx r)))
      (lam2 $ \d v -> app f (VTF.mult2 d (app fx v))) `com2` runFix

class Reify r x where
  reify :: x -> r h x

instance Lang r => Reify r () where
  reify _ = unit

instance Lang r => Reify r M.Double where
  reify = double

instance (Lang repr, Reify repr l, Reify repr r) => Reify repr (l, r) where
  reify (l, r) = mkProd2 (reify l) (reify r)

instance Lang r => Monoid r () where
  zero = unit
  plus = const1 $ const1 unit

instance Lang r => Group r () where
  invert = const1 unit
  minus = const1 $ const1 unit

instance Lang r => Vector r () where
  type Basis () = Void
  toFreeVector = const1 $ freeVector1 exfalso
  mult = const1 $ const1 unit
  divide = const1 $ const1 unit

instance Float r => Monoid r M.Float where
  zero = floatZero
  plus = floatPlus

instance Float r => Group r M.Float where
  minus = floatMinus

instance Lang r => Vector r M.Float where
  type Basis M.Float = ()
  toFreeVector = freeVector `com2` const `com2` float2Double
  mult = com2 floatMult double2Float
  divide = com2 (flip2 com double2Float) floatDivide

instance Lang r => Functor r (M.VTF.VectorTF b) where
  map = lam $ \f -> VTF.vtfMatch4 VTF.zero VTF.basis (lam2 $ \l r -> app f l `VTF.plus2` app f r) (lam2 $ \d x -> d `VTF.mult2` app f x)

instance (Prod repr, Monoid repr l, Monoid repr r) => Monoid repr (l, r) where
  zero = mkProd2 zero zero
  plus = lam2 $ \l r -> mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))

instance (Prod repr, Group repr l, Group repr r) => Group repr (l, r) where
  invert = bimap2 invert invert

instance (Prod repr, Double repr, Sum repr, FreeVector repr, Vector repr l, Vector repr r) => Vector repr (l, r) where
  type Basis (l, r) = M.Either (Basis l) (Basis r)
  toFreeVector = lam $ \p -> let_2 (toFreeVector1 $ zro1 p) $ lam $ \lfv -> let_2 (toFreeVector1 $ fst1 p) $ lam $ \rfv ->
    freeVector1 $ sumMatch2 (runFreeVector1 lfv) (runFreeVector1 rfv)
  mult = lam $ \x -> bimap2 (mult1 x) (mult1 x)

instance (Double r, Monoid r v) => Monoid r (M.Double -> v) where
  zero = const1 zero
  plus = lam3 $ \l r x -> plus2 (app l x) (app r x)

instance (Lang r, Group r v) => Group r (M.Double -> v) where
  invert = lam2 $ \l x -> app l (invert1 x)

instance (Lang r, Vector r v) => Vector r (M.Double -> v) where
  type Basis (M.Double -> v) = Basis v
  toFreeVector = lam $ \f -> toFreeVector1 $ app f (double 1)
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

instance Lang r => Monoid r (M.FreeVector b M.Double) where
  zero = freeVector1 $ const1 (double 0)
  plus = lam2 $ \l r -> freeVector1 $ lam $ \x -> runFreeVector2 l x `plus2` runFreeVector2 r x

instance Lang r => Group r (M.FreeVector b M.Double) where
  invert = lam $ \f -> freeVector1 $ lam $ \x -> invert1 (runFreeVector2 f x)
  minus = lam2 $ \l r -> freeVector1 $ lam $ \x -> runFreeVector2 l x `minus2` runFreeVector2 r x

instance Lang r => Vector r (M.FreeVector b M.Double) where
  type Basis (M.FreeVector b M.Double) = b
  toFreeVector = id
  mult = lam2 $ \d l -> freeVector1 $ lam $ \x -> d `mult2` runFreeVector2 l x
  divide = lam2 $ \l d -> freeVector1 $ lam $ \x -> runFreeVector2 l x `divide2` d

instance (MetaOrd b, Lang r) => Monoid r (FreeVectorBuilder b) where
  zero = Map.empty
  plus = Map.unionWith1 plus

instance (MetaOrd b, Lang r) => Group r (FreeVectorBuilder b) where
  invert = Map.mapMap1 invert

instance (MetaOrd b, Lang r) => Vector r (FreeVectorBuilder b) where
  type Basis (FreeVectorBuilder b) = b
  toFreeVector = buildFreeVector
  mult = Map.mapMap `com2` mult
  divide = lam2 $ \m d -> Map.mapMap2 (lam $ \x -> divide2 x d) m

instance Lang r => Monoid r (M.Fix (M.VTF.VectorTF b)) where
  zero = fix1 VTF.zero
  plus = lam2 $ \l r -> fix1 $ l `VTF.plus2` r

instance (MetaOrd b, Lang r) => Group r (M.Fix (M.VTF.VectorTF b)) where
  invert = mult1 (double (-1))

instance (MetaOrd b, Lang r) => Vector r (M.Fix (M.VTF.VectorTF b)) where
  type Basis (M.Fix (M.VTF.VectorTF b)) = b
  toFreeVector = buildFreeVector `com2` vtfCata1 (VTF.vtfMatch4 zero (flip2 Map.singleton (double 1)) plus mult)
  mult = lam $ \d -> fix `com2` VTF.mult1 d

instance (MetaOrd b, Lang r) => Monoid r (SVTFBuilder b) where
  zero = toSVTFBuilder1 VTF.zero
  plus = lam2 $ \l r -> l `bind2` (lam $ \lid -> r `bind2` (lam $ \rid -> toSVTFBuilder1 (VTF.plus2 lid rid)))

instance (MetaOrd b, Lang r) => Group r (SVTFBuilder b) where
  invert = mult1 (double (-1))

instance (MetaOrd b, Lang r) => Vector r (SVTFBuilder b) where
  type Basis (SVTFBuilder b) = b
  toFreeVector =
    buildFreeVector `com2` flip2 id Map.empty `com2`
    (lam $ \x -> zro `com2` (runState1 $ y2 (lam2 $ \fx i ->
      map2 (lam $ \m -> mkProd2 (get1 $ Map.lookup2 (fst1 x) i) (get1 $ Map.lookup2 m i)) getVar `bind2`
      (lam $ \p -> VTF.vtfMatch5
        (return1 zero)
        (lam $ \b -> return1 (Map.singleton2 b (fst1 p)))
        (lam2 $ \lid rid -> map2 (const1 zero) (update1 (updateWengert2 lid (fst1 p) `com2` updateWengert2 rid (fst1 p))))
        (lam2 $ \d xid -> map2 (const1 zero) (update1 (let_2 (d `mult2` (fst1 p)) (updateWengert1 xid))))
        (zro1 p) `bind2` (lam $ \fvb -> ite3 (return1 fvb) (map2 (plus1 fvb) $ app fx (pred1 i)) (eq2 i (int 0))))) (zro1 x))
    `com2` Map.insert2 (zro1 x) (double 0)) `com2` bimap2 id toMapR `com2` flip2 runState empty
  mult = lam2 $ \d x -> x `bind2` (lam $ \xid -> toSVTFBuilder1 (VTF.mult2 d xid))

type instance DiffType v (M.VTF.VectorTF t f) = M.VTF.VectorTF (DiffType v t) (DiffType v f)
instance (MetaOrd t, MetaOrd f) => MetaOrd (M.VTF.VectorTF t f) where
  diffOrd (_ :: Proxy (v, _)) = withDict (diffOrd $ Proxy @(v, t)) $ withDict (diffOrd $ Proxy @(v, f)) Dict

type instance DiffType v M.Int = M.Int
instance MetaOrd M.Int where
  diffOrd _ = Dict

instance Double r => Monoid r M.Double where
  zero = doubleZero
  plus = doublePlus

instance Double r => Group r M.Double where
  minus = doubleMinus

instance Lang r => Vector r M.Double where
  type Basis M.Double = ()
  toFreeVector = freeVector `com2` const
  mult = doubleMult
  divide = doubleDivide

uncurry1 = app uncurry
optionMatch2 = app2 optionMatch
optionMatch3 = app3 optionMatch
writer1 = app writer
runWriter1 = app runWriter
float2Double1 = app float2Double
state1 = app state
runState1 = app runState
runState2 = app2 runState
toSVTFBuilder1 = app toSVTFBuilder
double2Float1 = app double2Float
get1 = app get
return1 = app return
update1 = app update
updateWengert1 = app updateWengert
updateWengert2 = app2 updateWengert
vtfCata1 = app vtfCata
