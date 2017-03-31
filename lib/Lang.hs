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
    UndecidableSuperClasses,
    TypeOperators,
    TypeApplications,
    PartialTypeSignatures #-}

module Lang where
import DBI
import qualified Prelude as P
import Prelude (($), (.), (+), (-), (++), show, (>>=), (*), (/), undefined, Double)
import qualified Control.Monad.Writer as P
import qualified Data.Functor.Identity as P
import qualified GHC.Float as P
import qualified Data.Tuple as P
import Data.Void
import Data.Proxy
import Data.Proxy
import Data.Constraint
import Data.Constraint.Forall

type instance Diff v (P.Writer w a) = P.Writer (Diff v w) (Diff v a)
type instance Diff v Void = Void
type instance Diff v P.Double = (P.Double, v)
type instance Diff v P.Float = (P.Float, v)
type instance Diff v (P.Either a b) = P.Either (Diff v a) (Diff v b)
type instance Diff v (P.Maybe a) = P.Maybe (Diff v a)
type instance Diff v (P.IO a) = P.IO (Diff v a)
type instance Diff v [a] = [Diff v a]

class DBI repr => Lang repr where
  mkProd :: repr h (a -> b -> (a, b))
  zro :: repr h ((a, b) -> a)
  fst :: repr h ((a, b) -> b)
  double :: P.Double -> repr h P.Double
  doubleZero :: repr h P.Double
  doubleZero = double 0
  doubleOne :: repr h P.Double
  doubleOne = double 1
  doublePlus :: repr h (P.Double -> P.Double -> P.Double)
  doubleMinus :: repr h (P.Double -> P.Double -> P.Double)
  doubleMult :: repr h (P.Double -> P.Double -> P.Double)
  doubleDivide :: repr h (P.Double -> P.Double -> P.Double)
  doubleExp :: repr h (P.Double -> P.Double)
  float :: P.Float -> repr h P.Float
  floatZero :: repr h P.Float
  floatZero = float 0
  floatOne :: repr h P.Float
  floatOne = float 1
  floatPlus :: repr h (P.Float -> P.Float -> P.Float)
  floatMinus :: repr h (P.Float -> P.Float -> P.Float)
  floatMult :: repr h (P.Float -> P.Float -> P.Float)
  floatDivide :: repr h (P.Float -> P.Float -> P.Float)
  floatExp :: repr h (P.Float -> P.Float)
  fix :: repr h ((a -> a) -> a)
  left :: repr h (a -> P.Either a b)
  right :: repr h (b -> P.Either a b)
  sumMatch :: repr h ((a -> c) -> (b -> c) -> P.Either a b -> c)
  unit :: repr h ()
  exfalso :: repr h (Void -> a)
  nothing :: repr h (P.Maybe a)
  just :: repr h (a -> P.Maybe a)
  optionMatch :: repr h (b -> (a -> b) -> P.Maybe a -> b)
  ioRet :: repr h (a -> P.IO a)
  ioBind :: repr h (P.IO a -> (a -> P.IO b) -> P.IO b)
  ioMap :: repr h ((a -> b) -> P.IO a -> P.IO b)
  nil :: repr h [a]
  cons :: repr h (a -> [a] -> [a])
  listMatch :: repr h (b -> (a -> [a] -> b) -> [a] -> b)
  listAppend :: repr h ([a] -> [a] -> [a])
  listAppend = lam2 $ \l r -> fix2 (lam $ \self -> listMatch2 r (lam2 $ \a as -> cons2 a (app self as))) l
  writer :: repr h ((a, w) -> P.Writer w a)
  runWriter :: repr h (P.Writer w a -> (a, w))
  swap :: repr h ((l, r) -> (r, l))
  swap = lam $ \p -> mkProd2 (fst1 p) (zro1 p)
  curry :: repr h (((a, b) -> c) -> (a -> b -> c))
  uncurry :: repr h ((a -> b -> c) -> ((a, b) -> c))
  curry = lam3 $ \f a b -> app f (mkProd2 a b)
  uncurry = lam2 $ \f p -> app2 f (zro1 p) (fst1 p)
  float2Double :: repr h (P.Float -> P.Double)
  double2Float :: repr h (P.Double -> P.Float)

class Reify repr x where
  reify :: x -> repr h x

instance Lang repr => Reify repr () where
  reify _ = unit

instance Lang repr => Reify repr P.Double where
  reify = double

instance (Lang repr, Reify repr l, Reify repr r) => Reify repr (l, r) where
  reify (l, r) = mkProd2 (reify l) (reify r)

instance Lang Eval where
  zro = comb P.fst
  fst = comb P.snd
  mkProd = comb (,)
  double = comb
  doublePlus = comb (+)
  doubleMinus = comb (-)
  doubleMult = comb (*)
  doubleDivide = comb (/)
  fix = comb loop
    where loop x = x $ loop x
  left = comb P.Left
  right = comb P.Right
  sumMatch = comb $ \l r -> \case
                             P.Left x -> l x
                             P.Right x -> r x
  unit = comb ()
  exfalso = comb absurd
  nothing = comb P.Nothing
  just = comb P.Just
  ioRet = comb P.return
  ioBind = comb (>>=)
  nil = comb []
  cons = comb (:)
  listMatch = comb $ \l r -> \case
                            [] -> l
                            x:xs -> r x xs
  optionMatch = comb $ \l r -> \case
                              P.Nothing -> l
                              P.Just x -> r x
  ioMap = comb P.fmap
  writer = comb (P.WriterT . P.Identity)
  runWriter = comb P.runWriter
  doubleExp = comb P.exp
  float = comb
  floatPlus = comb (+)
  floatMinus = comb (-)
  floatMult = comb (*)
  floatDivide = comb (/)
  floatExp = comb P.exp
  float2Double = comb P.float2Double
  double2Float = comb P.double2Float

newtype UnHOAS repr h x = UnHOAS {runUnHOAS :: repr h x}

instance DBI repr => DBI (UnHOAS repr) where
  z = UnHOAS z
  s (UnHOAS x) = UnHOAS $ s x
  abs (UnHOAS x) = UnHOAS $ abs x
  app (UnHOAS f) (UnHOAS x) = UnHOAS $ app f x

instance Lang repr => Lang (UnHOAS repr) where
  mkProd = UnHOAS mkProd
  zro = UnHOAS zro
  fst = UnHOAS fst
  double = UnHOAS . double
  doublePlus = UnHOAS doublePlus
  doubleMinus = UnHOAS doubleMinus
  doubleMult = UnHOAS doubleMult
  doubleDivide = UnHOAS doubleDivide
  doubleExp = UnHOAS doubleExp
  fix = UnHOAS fix
  left = UnHOAS left
  right = UnHOAS right
  sumMatch = UnHOAS sumMatch
  unit = UnHOAS unit
  exfalso = UnHOAS exfalso
  nothing = UnHOAS nothing
  just = UnHOAS just
  ioRet = UnHOAS ioRet
  ioBind = UnHOAS ioBind
  nil = UnHOAS nil
  cons = UnHOAS cons
  listMatch = UnHOAS listMatch
  optionMatch = UnHOAS optionMatch
  ioMap = UnHOAS ioMap
  writer = UnHOAS writer
  runWriter = UnHOAS runWriter
  float = UnHOAS . float
  floatPlus = UnHOAS floatPlus
  floatMinus = UnHOAS floatMinus
  floatMult = UnHOAS floatMult
  floatDivide = UnHOAS floatDivide
  floatExp = UnHOAS floatExp
  float2Double = UnHOAS float2Double
  double2Float = UnHOAS double2Float

instance Lang Show where
  mkProd = name "mkProd"
  zro = name "zro"
  fst = name "fst"
  double = name . show
  doublePlus = name "plus"
  doubleMinus = name "minus"
  doubleMult = name "mult"
  doubleDivide = name "divide"
  doubleExp = name "exp"
  fix = name "fix"
  left = name "left"
  right = name "right"
  sumMatch = name "sumMatch"
  unit = name "unit"
  exfalso = name "exfalso"
  nothing = name "nothing"
  just = name "just"
  ioRet = name "ioRet"
  ioBind = name "ioBind"
  nil = name "nil"
  cons = name "cons"
  listMatch = name "listMatch"
  optionMatch = name "optionMatch"
  ioMap = name "ioMap"
  writer = name "writer"
  runWriter = name "runWriter"
  float = name . show
  floatPlus = name "plus"
  floatMinus = name "minus"
  floatMult = name "mult"
  floatDivide = name "divide"
  floatExp = name "exp"
  float2Double = name "float2Double"
  double2Float = name "double2Float"

instance Lang repr => Lang (GWDiff repr) where
  mkProd = GWDiff (P.const mkProd)
  zro = GWDiff $ P.const $ zro
  fst = GWDiff $ P.const $ fst
  double x = GWDiff $ P.const $ mkProd2 (double x) zero
  doublePlus = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  doubleMinus = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  doubleMult = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (mult2 (zro1 l) (zro1 r))
      (plus2 (mult2 (zro1 l) (fst1 r)) (mult2 (zro1 r) (fst1 l)))
  doubleDivide = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (zro1 r))
      (divide2 (minus2 (mult2 (zro1 r) (fst1 l)) (mult2 (zro1 l) (fst1 r)))
        (mult2 (zro1 r) (zro1 r)))
  doubleExp = GWDiff $ P.const $ lam $ \x -> mkProd2 (doubleExp1 (zro1 x)) (mult2 (doubleExp1 (zro1 x)) (fst1 x))
  fix = GWDiff $ P.const fix
  left = GWDiff $ P.const left
  right = GWDiff $ P.const right
  sumMatch = GWDiff $ P.const sumMatch
  unit = GWDiff $ P.const unit
  exfalso = GWDiff $ P.const exfalso
  nothing = GWDiff $ P.const nothing
  just = GWDiff $ P.const just
  ioRet = GWDiff $ P.const ioRet
  ioBind = GWDiff $ P.const ioBind
  nil = GWDiff $ P.const nil
  cons = GWDiff $ P.const cons
  listMatch = GWDiff $ P.const listMatch
  optionMatch = GWDiff $ P.const optionMatch
  ioMap = GWDiff $ P.const ioMap
  writer = GWDiff $ P.const writer
  runWriter = GWDiff $ P.const runWriter
  float x = GWDiff $ P.const $ mkProd2 (float x) zero
  floatPlus = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  floatMinus = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  floatMult = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (mult2 (float2Double1 (zro1 l)) (zro1 r))
      (plus2 (mult2 (float2Double1 (zro1 l)) (fst1 r)) (mult2 (float2Double1 (zro1 r)) (fst1 l)))
  floatDivide = GWDiff $ P.const $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (float2Double1 (zro1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (zro1 r)) (fst1 l)) (mult2 (float2Double1 (zro1 l)) (fst1 r)))
        (float2Double1 (mult2 (float2Double1 (zro1 r)) (zro1 r))))
  floatExp = GWDiff $ P.const $ lam $ \x -> mkProd2 (floatExp1 (zro1 x)) (mult2 (float2Double1 (floatExp1 (zro1 x))) (fst1 x))
  float2Double = GWDiff $ P.const $ bimap2 float2Double id
  double2Float = GWDiff $ P.const $ bimap2 double2Float id

instance (Vector repr v, Lang repr) => Lang (WDiff repr v) where
  mkProd = WDiff mkProd
  zro = WDiff zro
  fst = WDiff fst
  double x = WDiff $ mkProd2 (double x) zero
  doublePlus = WDiff $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  doubleMinus = WDiff $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  doubleMult = WDiff $ lam2 $ \l r ->
    mkProd2 (mult2 (zro1 l) (zro1 r))
      (plus2 (mult2 (zro1 l) (fst1 r)) (mult2 (zro1 r) (fst1 l)))
  doubleDivide = WDiff $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (zro1 r))
      (divide2 (minus2 (mult2 (zro1 r) (fst1 l)) (mult2 (zro1 l) (fst1 r)))
        (mult2 (zro1 r) (zro1 r)))
  doubleExp = WDiff $ lam $ \x -> mkProd2 (doubleExp1 (zro1 x)) (mult2 (doubleExp1 (zro1 x)) (fst1 x))
  fix = WDiff fix
  left = WDiff left
  right = WDiff right
  sumMatch = WDiff sumMatch
  unit = WDiff unit
  exfalso = WDiff exfalso
  nothing = WDiff nothing
  just = WDiff just
  ioRet = WDiff ioRet
  ioBind = WDiff ioBind
  nil = WDiff nil
  cons = WDiff cons
  listMatch = WDiff listMatch
  optionMatch = WDiff optionMatch
  ioMap = WDiff ioMap
  writer = WDiff writer
  runWriter = WDiff runWriter
  float x = WDiff $ mkProd2 (float x) zero
  floatPlus = WDiff $ lam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  floatMinus = WDiff $ lam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  floatMult = WDiff $ lam2 $ \l r ->
    mkProd2 (mult2 (float2Double1 (zro1 l)) (zro1 r))
      (plus2 (mult2 (float2Double1 (zro1 l)) (fst1 r)) (mult2 (float2Double1 (zro1 r)) (fst1 l)))
  floatDivide = WDiff $ lam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (float2Double1 (zro1 r)))
      (divide2 (minus2 (mult2 (float2Double1 (zro1 r)) (fst1 l)) (mult2 (float2Double1 (zro1 l)) (fst1 r)))
        (float2Double1 (mult2 (float2Double1 (zro1 r)) (zro1 r))))
  floatExp = WDiff $ lam $ \x -> mkProd2 (floatExp1 (zro1 x)) (mult2 (float2Double1 (floatExp1 (zro1 x))) (fst1 x))
  float2Double = WDiff $ bimap2 float2Double id
  double2Float = WDiff $ bimap2 double2Float id

instance Lang repr => ProdCon (Monoid repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (WithDiff repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Reify repr) l r where prodCon = Sub Dict

instance Lang repr => ProdCon (Vector repr) l r where prodCon = Sub Dict

instance Lang repr => Lang (ImpW repr) where
  nil = NoImpW nil
  cons = NoImpW cons
  listMatch = NoImpW listMatch
  zro = NoImpW zro
  fst = NoImpW fst
  mkProd = NoImpW mkProd
  ioRet = NoImpW ioRet
  ioMap = NoImpW ioMap
  ioBind = NoImpW ioBind
  unit = NoImpW unit
  nothing = NoImpW nothing
  just = NoImpW just
  optionMatch = NoImpW optionMatch
  exfalso = NoImpW exfalso
  fix = NoImpW fix
  left = NoImpW left
  right = NoImpW right
  sumMatch = NoImpW sumMatch
  writer = NoImpW writer
  runWriter = NoImpW runWriter
  double = NoImpW . double
  doubleExp = NoImpW doubleExp
  doublePlus = NoImpW doublePlus
  doubleMinus = NoImpW doubleMinus
  doubleMult = NoImpW doubleMult
  doubleDivide = NoImpW doubleDivide
  float = NoImpW . float
  floatExp = NoImpW floatExp
  floatPlus = NoImpW floatPlus
  floatMinus = NoImpW floatMinus
  floatMult = NoImpW floatMult
  floatDivide = NoImpW floatDivide
  float2Double = NoImpW float2Double
  double2Float = NoImpW double2Float

instance (Lang l, Lang r) => Lang (Combine l r) where
  mkProd = Combine mkProd mkProd
  zro = Combine zro zro
  fst = Combine fst fst
  double x = Combine (double x) (double x)
  doublePlus = Combine doublePlus doublePlus
  doubleMinus = Combine doubleMinus doubleMinus
  doubleMult = Combine doubleMult doubleMult
  doubleDivide = Combine doubleDivide doubleDivide
  doubleExp = Combine doubleExp doubleExp
  float x = Combine (float x) (float x)
  floatPlus = Combine floatPlus floatPlus
  floatMinus = Combine floatMinus floatMinus
  floatMult = Combine floatMult floatMult
  floatDivide = Combine floatDivide floatDivide
  floatExp = Combine floatExp floatExp
  fix = Combine fix fix
  left = Combine left left
  right = Combine right right
  sumMatch = Combine sumMatch sumMatch
  unit = Combine unit unit
  exfalso = Combine exfalso exfalso
  nothing = Combine nothing nothing
  just = Combine just just
  optionMatch = Combine optionMatch optionMatch
  ioRet = Combine ioRet ioRet
  ioBind = Combine ioBind ioBind
  ioMap = Combine ioMap ioMap
  nil = Combine nil nil
  cons = Combine cons cons
  listMatch = Combine listMatch listMatch
  runWriter = Combine runWriter runWriter
  writer = Combine writer writer
  double2Float = Combine double2Float double2Float
  float2Double = Combine float2Double float2Double

instance Lang repr => WithDiff repr () where
  withDiff = const1 id

instance Lang repr => WithDiff repr P.Double where
  withDiff = lam2 $ \conv d -> mkProd2 d (app conv doubleOne)

instance (Lang repr, WithDiff repr l, WithDiff repr r) => WithDiff repr (l, r) where
  withDiff = lam $ \conv -> bimap2 (withDiff1 (lam $ \l -> app conv (mkProd2 l zero))) (withDiff1 (lam $ \r -> app conv (mkProd2 zero r)))

class Monoid r g => Group r g where
  invert :: r h (g -> g)
  minus :: r h (g -> g -> g)
  default invert :: Lang r => r h (g -> g)
  invert = minus1 zero
  default minus :: Lang r => r h (g -> g -> g)
  minus = lam2 $ \x y -> plus2 x (invert1 y)
  {-# MINIMAL (invert | minus) #-}

class Group r v => Vector r v where
  mult :: r h (P.Double -> v -> v)
  divide :: r h (v -> P.Double -> v)
  default mult :: Lang r => r h (P.Double -> v -> v)
  mult = lam2 $ \x y -> divide2 y (recip1 x)
  default divide :: Lang r => r h (v -> P.Double -> v)
  divide = lam2 $ \x y -> mult2 (recip1 y) x
  {-# MINIMAL (mult | divide) #-}

instance Lang r => Monoid r () where
  zero = unit
  plus = const1 $ const1 unit

instance Lang r => Group r () where
  invert = const1 unit
  minus = const1 $ const1 unit

instance Lang r => Vector r () where
  mult = const1 $ const1 unit
  divide = const1 $ const1 unit

instance Lang r => Monoid r P.Double where
  zero = doubleZero
  plus = doublePlus

instance Lang r => Group r P.Double where
  minus = doubleMinus

instance Lang r => Vector r P.Double where
  mult = doubleMult
  divide = doubleDivide

instance Lang r => Monoid r P.Float where
  zero = floatZero
  plus = floatPlus

instance Lang r => Group r P.Float where
  minus = floatMinus

instance Lang r => Vector r P.Float where
  mult = com2 floatMult double2Float
  divide = com2 (flip2 com double2Float) floatDivide

instance (Lang repr, Monoid repr l, Monoid repr r) => Monoid repr (l, r) where
  zero = mkProd2 zero zero
  plus = lam2 $ \l r -> mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))

instance (Lang repr, Group repr l, Group repr r) => Group repr (l, r) where
  invert = bimap2 invert invert

instance (Lang repr, Vector repr l, Vector repr r) => Vector repr (l, r) where
  mult = lam $ \x -> bimap2 (mult1 x) (mult1 x)

instance (Lang repr, Monoid repr v) => Monoid repr (Double -> v) where
  zero = const1 zero
  plus = lam3 $ \l r x -> plus2 (app l x) (app r x)

instance (Lang repr, Group repr v) => Group repr (Double -> v) where
  invert = lam2 $ \l x -> app l (invert1 x)

instance (Lang repr, Vector repr v) => Vector repr (Double -> v) where
  mult = lam3 $ \l r x -> app r (mult2 l x)

instance Lang r => Monoid r [a] where
  zero = nil
  plus = listAppend

instance Lang r => Functor r [] where
  map = lam $ \f -> fix1 $ lam $ \self -> listMatch2 nil (lam2 $ \x xs -> cons2 (app f x) $ app self xs)

instance Lang r => BiFunctor r (,) where
  bimap = lam3 $ \l r p -> mkProd2 (app l (zro1 p)) (app r (fst1 p))

instance Lang r => Functor r (P.Writer w) where
  map = lam $ \f -> com2 writer (com2 (bimap2 f id) runWriter)

instance (Lang r, Monoid r w) => Applicative r (P.Writer w) where
  pure = com2 writer (flip2 mkProd zero)
  ap = lam2 $ \f x -> writer1 (mkProd2 (app (zro1 (runWriter1 f)) (zro1 (runWriter1 x))) (plus2 (fst1 (runWriter1 f)) (fst1 (runWriter1 x))))

instance (Lang r, Monoid r w) => Monad r (P.Writer w) where
  join = lam $ \x -> writer1 $ mkProd2 (zro1 $ runWriter1 $ zro1 $ runWriter1 x) (plus2 (fst1 $ runWriter1 $ zro1 $ runWriter1 x) (fst1 $ runWriter1 x))

instance Lang r => Functor r P.IO where
  map = ioMap

instance Lang r => Applicative r P.IO where
  pure = ioRet
  ap = lam2 $ \f x -> ioBind2 f (flip2 ioMap x)

instance Lang r => Monad r P.IO where
  bind = ioBind

instance Lang r => Functor r P.Maybe where
  map = lam $ \func -> optionMatch2 nothing (com2 just func)

instance Lang r => Applicative r P.Maybe where
  pure = just
  ap = optionMatch2 (const1 nothing) map

instance Lang r => Monad r P.Maybe where
  bind = lam2 $ \x func -> optionMatch3 nothing func x

runImpW :: forall repr h x. Lang repr => ImpW repr h x -> RunImpW repr h x
runImpW (ImpW x) = RunImpW x
runImpW (NoImpW x) = RunImpW (const1 x :: repr h (() -> x))

newtype GWDiff repr h x = GWDiff {runGWDiff :: forall v. Vector repr v => Proxy v -> repr (Diff v h) (Diff v x)}

instance DBI repr => DBI (GWDiff repr) where
  z = GWDiff (P.const z)
  s (GWDiff x) = GWDiff (\p -> s $ x p)
  app (GWDiff f) (GWDiff x) = GWDiff (\p -> app (f p) (x p))
  abs (GWDiff x) = GWDiff (\p -> abs $ x p)

cons2 = app2 cons
listMatch2 = app2 listMatch
fix1 = app fix
fix2 = app2 fix
uncurry1 = app uncurry
optionMatch2 = app2 optionMatch
optionMatch3 = app3 optionMatch
zro1 = app zro
fst1 = app fst
mult1 = app mult
mult2 = app2 mult
divide2 = app2 divide
invert1 = app invert
mkProd1 = app mkProd
mkProd2 = app2 mkProd
minus1 = app minus
divide1 = app divide
recip = divide1 doubleOne
recip1 = app recip
writer1 = app writer
runWriter1 = app runWriter
ioBind2 = app2 ioBind
minus2 = app2 minus
float2Double1 = app float2Double
doubleExp1 = app doubleExp
floatExp1 = app floatExp

instance Lang repr => DBI (ImpW repr) where
  z = NoImpW z
  s :: forall a h b. ImpW repr h b -> ImpW repr (a, h) b
  s (ImpW x) = work x
    where
      work :: Weight w => repr h (w -> b) -> ImpW repr (a, h) b
      work x = ImpW (s x)
  s (NoImpW x) = NoImpW (s x)
  app (ImpW f) (ImpW x) = ImpW (lam $ \p -> app (app (conv f) (zro1 p)) (app (conv x) (fst1 p)))
  app (NoImpW f) (NoImpW x) = NoImpW (app f x)
  app (ImpW f) (NoImpW x) = ImpW (lam $ \w -> app2 (conv f) w (conv x))
  app (NoImpW f) (ImpW x) = ImpW (lam $ \w -> app (conv f) (app (conv x) w))
  abs (ImpW f) = ImpW (flip1 $ abs f)
  abs (NoImpW x) = NoImpW (abs x)
