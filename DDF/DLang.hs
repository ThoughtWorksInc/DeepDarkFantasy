{-# LANGUAGE
  MultiParamTypeClasses,
  RankNTypes,
  ScopedTypeVariables,
  FlexibleInstances,
  FlexibleContexts,
  UndecidableInstances,
  PolyKinds,
  LambdaCase,
  NoMonomorphismRestriction,
  TypeFamilies,
  LiberalTypeSynonyms,
  FunctionalDependencies,
  ExistentialQuantification,
  InstanceSigs,
  TupleSections,
  ConstraintKinds,
  DefaultSignatures,
  TypeOperators,
  TypeApplications,
  PartialTypeSignatures,
  NoImplicitPrelude
#-}

module DDF.DLang (module DDF.DLang, module DDF.Bool, module DDF.Char, module DDF.Double, module DDF.Float, module DDF.Bimap, module DDF.Dual, module DDF.Diff) where
import DDF.Bool
import DDF.Char
import DDF.Double
import DDF.Float
import DDF.Bimap
import DDF.Dual
import DDF.Vector
import DDF.Diff

import qualified DDF.Meta.Dual as M
import qualified Control.Monad.Writer as M (Writer)
import qualified GHC.Float as M
import qualified Prelude as M
import qualified Data.Map as M
import qualified DDF.Map as Map

class (Bool r, Char r, Double r, Float r, Bimap r, Dual r) => DLang r where
  fix :: r h ((a -> a) -> a)
  left :: r h (a -> M.Either a b)
  right :: r h (b -> M.Either a b)
  sumMatch :: r h ((a -> c) -> (b -> c) -> M.Either a b -> c)
  unit :: r h ()
  exfalso :: r h (Void -> a)
  ioRet :: r h (a -> M.IO a)
  ioBind :: r h (M.IO a -> (a -> M.IO b) -> M.IO b)
  ioMap :: r h ((a -> b) -> M.IO a -> M.IO b)
  nil :: r h [a]
  cons :: r h (a -> [a] -> [a])
  listMatch :: r h (b -> (a -> [a] -> b) -> [a] -> b)
  listAppend :: r h ([a] -> [a] -> [a])
  listAppend = lam2 $ \l r -> fix2 (lam $ \self -> listMatch2 r (lam2 $ \a as -> cons2 a (app self as))) l
  writer :: r h ((a, w) -> M.Writer w a)
  runWriter :: r h (M.Writer w a -> (a, w))
  float2Double :: r h (M.Float -> M.Double)
  double2Float :: r h (M.Double -> M.Float)
  undefined :: r h a
  undefined = fix1 id
  state :: r h ((x -> (y, x)) -> State x y)
  runState :: r h (State x y -> (x -> (y, x)))
  putStrLn :: r h (String -> IO ())
  nextDiff :: (DiffVector r v, RTDiff r x, RTDiff r v) => Proxy v -> r h (InfDiff Eval () x -> InfDiff Eval () (Diff v x))
  infDiffGet :: RTDiff r x => r h (InfDiff Eval () x -> x)
  rtDiffDiff :: forall v x. (DiffVector r v, RTDiff r v) => Proxy r -> Proxy (v, x) -> RTDiff r x :- RTDiff r (Diff v x)
  intDLang :: Proxy r -> Dict (DLang (DiffInt r))
  litInfDiff :: DiffInt r () x -> r h (InfDiff Eval () x)
  infDiffApp :: r h (InfDiff Eval () (a -> b) -> InfDiff Eval () a -> InfDiff Eval () b)
  infDiffAppApp :: r h (InfDiff Eval () (a -> b -> c) -> InfDiff Eval () a -> InfDiff Eval () b -> InfDiff Eval () c)
  infDiffAppApp = lam2 $ \f x -> infDiffApp1 $ infDiffApp2 f x
  rtdd :: Proxy r -> Dict (RTDiff r M.Double)

class Reify r x where
  reify :: x -> r h x

instance DLang r => Reify r () where
  reify _ = unit

instance DLang r => Reify r M.Double where
  reify = double

instance (DLang repr, Reify repr l, Reify repr r) => Reify repr (l, r) where
  reify (l, r) = mkProd2 (reify l) (reify r)

instance DLang repr => ProdCon (Monoid repr) l r where prodCon = Sub Dict

instance DLang repr => ProdCon (Reify repr) l r where prodCon = Sub Dict

instance DLang repr => ProdCon (Vector repr) l r where prodCon = Sub Dict

instance DLang r => Monoid r () where
  zero = unit
  plus = const1 $ const1 unit

instance DLang r => Group r () where
  invert = const1 unit
  minus = const1 $ const1 unit

instance DLang r => Vector r () where
  mult = const1 $ const1 unit
  divide = const1 $ const1 unit

instance Float r => Monoid r M.Float where
  zero = floatZero
  plus = floatPlus

instance Float r => Group r M.Float where
  minus = floatMinus

instance DLang r => Vector r M.Float where
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

instance (DLang r, Group r v) => Group r (M.Double -> v) where
  invert = lam2 $ \l x -> app l (invert1 x)

instance (DLang r, Vector r v) => Vector r (M.Double -> v) where
  mult = lam3 $ \l r x -> app r (mult2 l x)

instance DLang r => Monoid r [a] where
  zero = nil
  plus = listAppend

instance DLang r => Functor r [] where
  map = lam $ \f -> fix1 $ lam $ \self -> listMatch2 nil (lam2 $ \x xs -> cons2 (app f x) $ app self xs)

instance DLang r => BiFunctor r Either where
  bimap = lam2 $ \l r -> sumMatch2 (com2 left l) (com2 right r)

instance Prod r => BiFunctor r (,) where
  bimap = lam3 $ \l r p -> mkProd2 (app l (zro1 p)) (app r (fst1 p))

instance Dual r => BiFunctor r M.Dual where
  bimap = lam2 $ \l r -> dual `com2` bimap2 l r `com2` runDual

instance DLang r => Functor r (Writer w) where
  map = lam $ \f -> com2 writer (com2 (bimap2 f id) runWriter)

instance DLang r => Functor r (M.Map k) where
  map = Map.mapMap

instance (DLang r, Monoid r w) => Applicative r (Writer w) where
  pure = com2 writer (flip2 mkProd zero)
  ap = lam2 $ \f x -> writer1 (mkProd2 (app (zro1 (runWriter1 f)) (zro1 (runWriter1 x))) (plus2 (fst1 (runWriter1 f)) (fst1 (runWriter1 x))))

instance (DLang r, Monoid r w) => Monad r (Writer w) where
  join = lam $ \x -> writer1 $ mkProd2 (zro1 $ runWriter1 $ zro1 $ runWriter1 x) (plus2 (fst1 $ runWriter1 $ zro1 $ runWriter1 x) (fst1 $ runWriter1 x))

instance DLang r => Functor r (State l) where
  map = lam2 $ \f st -> state1 (com2 (bimap2 f id) (runState1 st))

instance DLang r => Applicative r (State l) where
  pure = lam $ \x -> state1 (mkProd1 x)
  ap = lam2 $ \f x -> state1 $ lam $ \st -> let_2 (runState2 f st) (lam $ \p -> bimap3 (zro1 p) id (runState2 x (fst1 p)))

instance DLang r => Monad r (State l) where
  join = lam $ \x -> state1 $ lam $ \st -> let_2 (runState2 x st) (uncurry1 runState)

instance DLang r => Functor r M.IO where
  map = ioMap

instance DLang r => Applicative r M.IO where
  pure = ioRet
  ap = lam2 $ \f x -> ioBind2 f (flip2 ioMap x)

instance DLang r => Monad r M.IO where
  bind = ioBind

instance DLang r => Functor r M.Maybe where
  map = lam $ \func -> optionMatch2 nothing (com2 just func)

instance DLang r => Applicative r M.Maybe where
  pure = just
  ap = optionMatch2 (const1 nothing) map

instance DLang r => Monad r M.Maybe where
  bind = lam2 $ \x func -> optionMatch3 nothing func x

cons2 = app2 cons
listMatch2 = app2 listMatch
fix1 = app fix
fix2 = app2 fix
uncurry1 = app uncurry
optionMatch2 = app2 optionMatch
optionMatch3 = app3 optionMatch
writer1 = app writer
runWriter1 = app runWriter
ioBind2 = app2 ioBind
float2Double1 = app float2Double
doubleExp1 = app doubleExp
floatExp1 = app floatExp
sumMatch2 = app2 sumMatch
state1 = app state
runState1 = app runState
runState2 = app2 runState
infDiffApp1 = app infDiffApp
infDiffApp2 = app2 infDiffApp
infDiffAppApp1 = app infDiffAppApp
nextDiff1 p = app $ nextDiff p