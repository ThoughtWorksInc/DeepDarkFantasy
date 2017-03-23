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
    EmptyCase,
    FunctionalDependencies,
    ExistentialQuantification,
    InstanceSigs,
    AllowAmbiguousTypes,
    TupleSections,
    ConstraintKinds #-}

module DBI where
import qualified Prelude as P
import Prelude (($), (.), (+), (-), (++), show, (>>=), (*), (/), undefined)
import Util
import Data.Void
import Control.Monad (when)
import qualified Control.Monad.Writer as P
import qualified Data.Functor.Identity as P
import qualified Data.Tuple as P
import System.Random
import Data.Proxy

instance Random () where
  random = ((),)
  randomR _ = random

instance (Random l, Random r) => Random (l, r) where
  random g0 = ((l, r), g2)
    where
      (l, g1) = random g0
      (r, g2) = random g1
  randomR ((llo, rlo), (lhi, rhi)) g0 = ((l, r), g2)
    where
      (l, g1) = randomR (llo, lhi) g0
      (r, g2) = randomR (rlo, rhi) g1

class Reify repr x where
  reify :: x -> repr h x

instance DBI repr => Reify repr () where
  reify _ = unit

instance DBI repr => Reify repr P.Double where
  reify = lit

instance (DBI repr, Reify repr l, Reify repr r) => Reify repr (l, r) where
  reify (l, r) = mkProd2 (reify l) (reify r)

class DBI repr where
  z :: repr (a, h) a
  s :: repr h b -> repr (a, h) b
  lam :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b
  mkProd :: repr h (a -> b -> (a, b))
  zro :: repr h ((a, b) -> a)
  fst :: repr h ((a, b) -> b)
  lit :: P.Double -> repr h P.Double
  litZero :: repr h P.Double
  litZero = lit 0
  litOne :: repr h P.Double
  litOne = lit 1
  doublePlus :: repr h (P.Double -> P.Double -> P.Double)
  doubleMinus :: repr h (P.Double -> P.Double -> P.Double)
  doubleMult :: repr h (P.Double -> P.Double -> P.Double)
  doubleDivide :: repr h (P.Double -> P.Double -> P.Double)
  hoas :: (repr (a, h) a -> repr (a, h) b) -> repr h (a -> b)
  hoas f = lam $ f z
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
  com :: repr h ((b -> c) -> (a -> b) -> (a -> c))
  com = hlam3 $ \f g x -> app f (app g x)
  listAppend :: repr h ([a] -> [a] -> [a])
  listAppend = hlam2 $ \l r -> fix2 (hlam $ \self -> listMatch2 r (hlam2 $ \a as -> cons2 a (app self as))) l
  writer :: repr h ((a, w) -> P.Writer w a)
  runWriter :: repr h (P.Writer w a -> (a, w))
  swap :: repr h ((l, r) -> (r, l))
  swap = hlam $ \p -> mkProd2 (fst1 p) (zro1 p)
  flip :: repr h ((a -> b -> c) -> (b -> a -> c))
  flip = hlam3 $ \f b a -> app2 f a b
  id :: repr h (a -> a)
  id = hlam $ \x -> x
  const :: repr h (a -> b -> a)
  const = hlam2 $ \x _ -> x
  scomb :: repr h ((a -> b -> c) -> (a -> b) -> (a -> c))
  scomb = hlam3 $ \f x arg -> app (app f arg) (app x arg)
  exp :: repr h (P.Double -> P.Double)
  curry :: repr h (((a, b) -> c) -> (a -> b -> c))
  uncurry :: repr h ((a -> b -> c) -> ((a, b) -> c))
  curry = hlam3 $ \f a b -> app f (mkProd2 a b)
  uncurry = hlam2 $ \f p -> app2 f (zro1 p) (fst1 p)

const1 = app const
cons2 = app2 cons
listMatch2 = app2 listMatch
fix1 = app fix
fix2 = app2 fix
uncurry1 = app uncurry

class Monoid r m where
  zero :: r h m
  plus :: r h (m -> m -> m)

class (DBI r, Monoid r g) => Group r g where
  invert :: r h (g -> g)
  minus :: r h (g -> g -> g)
  invert = minus1 zero
  minus = hlam2 $ \x y -> plus2 x (invert1 y)
  {-# MINIMAL (invert | minus) #-}

minus1 = app minus
divide1 = app divide

recip = divide1 litOne
recip1 = app recip

class Group r v => Vector r v where
  mult :: r h (P.Double -> v -> v)
  divide :: r h (v -> P.Double -> v)
  mult = hlam2 $ \x y -> divide2 y (recip1 x)
  divide = hlam2 $ \x y -> mult2 (recip1 y) x
  {-# MINIMAL (mult | divide) #-}

instance DBI r => Monoid r () where
  zero = unit
  plus = const1 $ const1 unit

instance DBI r => Group r () where
  invert = const1 unit
  minus = const1 $ const1 unit

instance DBI r => Vector r () where
  mult = const1 $ const1 unit
  divide = const1 $ const1 unit

instance DBI r => Monoid r P.Double where
  zero = litZero
  plus = doublePlus

instance DBI r => Group r P.Double where
  minus = doubleMinus

instance DBI r => Vector r P.Double where
  mult = doubleMult
  divide = doubleDivide

instance (DBI repr, Monoid repr l, Monoid repr r) => Monoid repr (l, r) where
  zero = mkProd2 zero zero
  plus = hlam2 $ \l r -> mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))

instance (DBI repr, Group repr l, Group repr r) => Group repr (l, r) where
  invert = bimap2 invert invert

instance (DBI repr, Vector repr l, Vector repr r) => Vector repr (l, r) where
  mult = hlam $ \x -> bimap2 (mult1 x) (mult1 x)

instance (DBI repr, Monoid repr l, Monoid repr r) => Monoid repr (l -> r) where
  zero = const1 zero
  plus = hlam3 $ \l r x -> plus2 (app l x) (app r x)

instance (DBI repr, Group repr l, Group repr r) => Group repr (l -> r) where
  invert = hlam2 $ \l x -> app l (invert1 x)

instance (DBI repr, Vector repr l, Vector repr r) => Vector repr (l -> r) where
  mult = hlam3 $ \l r x -> app r (mult2 l x)

instance DBI r => Monoid r [a] where
  zero = nil
  plus = listAppend

class Functor r f where
  map :: r h ((a -> b) -> (f a -> f b))

instance DBI r => Functor r [] where
  map = hlam $ \f -> fix1 $ hlam $ \self -> listMatch2 nil (hlam2 $ \x xs -> cons2 (app f x) $ app self xs)

map2 = app2 map

class Functor r a => Applicative r a where
  pure :: r h (x -> a x)
  ap :: r h (a (x -> y) -> a x -> a y)

return = pure

class (DBI r, Applicative r m) => Monad r m where
  bind :: r h (m a -> (a -> m b) -> m b)
  join :: r h (m (m a) -> m a)
  join = hlam $ \m -> bind2 m id
  bind = hlam2 $ \m f -> join1 (app2 map f m)
  {-# MINIMAL (join | bind) #-}

bind2 = app2 bind
map1 = app map
join1 = app join
bimap2 = app2 bimap
flip1 = app flip
flip2 = app2 flip

class DBI r => BiFunctor r p where
  bimap :: r h ((a -> b) -> (c -> d) -> p a c -> p b d)

instance DBI r => BiFunctor r (,) where
  bimap = hlam3 $ \l r p -> mkProd2 (app l (zro1 p)) (app r (fst1 p))

instance DBI r => Functor r (P.Writer w) where
  map = hlam $ \f -> com2 writer (com2 (bimap2 f id) runWriter)

writer1 = app writer
runWriter1 = app runWriter

instance (DBI r, Monoid r w) => Applicative r (P.Writer w) where
  pure = com2 writer (flip2 mkProd zero)
  ap = hlam2 $ \f x -> writer1 (mkProd2 (app (zro1 (runWriter1 f)) (zro1 (runWriter1 x))) (plus2 (fst1 (runWriter1 f)) (fst1 (runWriter1 x))))

instance (DBI r, Monoid r w) => Monad r (P.Writer w) where
  join = hlam $ \x -> writer1 $ mkProd2 (zro1 $ runWriter1 $ zro1 $ runWriter1 x) (plus2 (fst1 $ runWriter1 $ zro1 $ runWriter1 x) (fst1 $ runWriter1 x))

instance DBI r => Functor r P.IO where
  map = ioMap

ioBind2 = app2 ioBind

instance DBI r => Applicative r P.IO where
  pure = ioRet
  ap = hlam2 $ \f x -> ioBind2 f (flip2 ioMap x)

instance DBI r => Monad r P.IO where
  bind = ioBind

app3 f x y z = app (app2 f x y) z

optionMatch2 = app2 optionMatch
optionMatch3 = app3 optionMatch
com2 = app2 com

instance DBI r => Functor r P.Maybe where
  map = hlam $ \func -> optionMatch2 nothing (com2 just func)

instance DBI r => Applicative r P.Maybe where
  pure = just
  ap = optionMatch2 (const1 nothing) map

instance DBI r => Monad r P.Maybe where
  bind = hlam2 $ \x func -> optionMatch3 nothing func x

newtype Eval h x = Eval {runEval :: h -> x}

comb = Eval . P.const

instance DBI Eval where
  z = Eval P.fst
  s (Eval a) = Eval $ a . P.snd
  lam (Eval f) = Eval $ \a h -> f (h, a)
  app (Eval f) (Eval x) = Eval $ \h -> f h $ x h
  zro = comb P.fst
  fst = comb P.snd
  mkProd = comb (,)
  lit = comb
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
  exp = comb P.exp

data AST = Leaf P.String | App P.String AST [AST] | Lam P.String [P.String] AST

appAST (Leaf f) x = App f x []
appAST (App f x l) r = App f x (l ++ [r])
appAST lam r = appAST (Leaf $ show lam) r

lamAST str (Lam s l t) = Lam str (s:l) t
lamAST str r = Lam str [] r

instance P.Show AST where
  show (Leaf f) = f
  show (App f x l) = "(" ++ f ++ " " ++ show x ++ P.concatMap ((" " ++) . show) l ++ ")"
  show (Lam s l t) = "(\\" ++ s ++ P.concatMap (" " ++) l ++ " -> " ++ show t ++ ")"
newtype Show h a = Show {runShow :: [P.String] -> P.Int -> AST}
name = Show . P.const . P.const . Leaf

instance DBI Show where
  z = Show $ P.const $ Leaf . show . P.flip (-) 1
  s (Show v) = Show $ \vars -> v vars . P.flip (-) 1
  lam (Show f) = Show $ \vars x -> lamAST (show x) (f vars (x + 1))
  app (Show f) (Show x) = Show $ \vars h -> appAST (f vars h) (x vars h)
  hoas f = Show $ \(v:vars) h ->
    lamAST v (runShow (f $ Show $ P.const $ P.const $ Leaf v) vars (h + 1))
  mkProd = name "mkProd"
  zro = name "zro"
  fst = name "fst"
  lit = name . show
  doublePlus = name "plus"
  doubleMinus = name "minus"
  doubleMult = name "mult"
  doubleDivide = name "divide"
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
  exp = name "exp"

class NT repr l r where
    conv :: repr l t -> repr r t

instance {-# INCOHERENT #-} (DBI repr, NT repr l r) => NT repr l (a, r) where
    conv = s . conv

instance NT repr x x where
    conv = P.id

hlam :: forall repr a b h. DBI repr =>
  ((forall k. NT repr (a, h) k => repr k a) -> (repr (a, h)) b) -> repr h (a -> b)
hlam f = hoas (\x -> f $ conv x)

hlam2 :: forall repr a b c h. DBI repr =>
  ((forall k. NT repr (a, h) k => repr k a) -> (forall k. NT repr (b, (a, h)) k => repr k b) -> (repr (b, (a, h))) c) -> repr h (a -> b -> c)
hlam2 f = hlam $ \x -> hlam $ \y -> f x y

hlam3 f = hlam2 $ \x y -> hlam $ \z -> f x y z

type family Diff v x
type instance Diff v P.Double = (P.Double, v)
type instance Diff v () = ()
type instance Diff v (a, b) = (Diff v a, Diff v b)
type instance Diff v (a -> b) = Diff v a -> Diff v b
type instance Diff v (P.Either a b) = P.Either (Diff v a) (Diff v b)
type instance Diff v Void = Void
type instance Diff v (P.Maybe a) = P.Maybe (Diff v a)
type instance Diff v (P.IO a) = P.IO (Diff v a)
type instance Diff v [a] = [Diff v a]
type instance Diff v (P.Writer w a) = P.Writer (Diff v w) (Diff v a)

newtype WDiff repr v h x = WDiff {runWDiff :: repr (Diff v h) (Diff v x)}

app2 f a = app (app f a)

mkProd1 = app mkProd
mkProd2 = app2 mkProd
plus2 = app2 plus
zro1 = app zro
fst1 = app fst
minus2 = app2 minus
mult1 = app mult
mult2 = app2 mult
divide2 = app2 divide
invert1 = app invert

instance (Vector repr v, DBI repr) => DBI (WDiff repr v) where
  z = WDiff z
  s (WDiff x) = WDiff $ s x
  lam (WDiff f) = WDiff $ lam f
  app (WDiff f) (WDiff x) = WDiff $ app f x
  mkProd = WDiff mkProd
  zro = WDiff zro
  fst = WDiff fst
  lit x = WDiff $ mkProd2 (lit x) zero
  doublePlus = WDiff $ hlam2 $ \l r ->
    mkProd2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  doubleMinus = WDiff $ hlam2 $ \l r ->
    mkProd2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  doubleMult = WDiff $ hlam2 $ \l r ->
    mkProd2 (mult2 (zro1 l) (zro1 r))
      (plus2 (mult2 (zro1 l) (fst1 r)) (mult2 (zro1 r) (fst1 l)))
  doubleDivide = WDiff $ hlam2 $ \l r ->
    mkProd2 (divide2 (zro1 l) (zro1 r))
      (divide2 (minus2 (mult2 (zro1 r) (fst1 l)) (mult2 (zro1 l) (fst1 r)))
        (mult2 (zro1 r) (zro1 r)))
  hoas f = WDiff $ hoas (runWDiff . f . WDiff)
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
  exp = WDiff $ hlam $ \x -> mkProd2 (exp1 (zro1 x)) (mult2 (exp1 (zro1 x)) (fst1 x))

exp1 = app exp

noEnv :: repr () x -> repr () x
noEnv = P.id

selfWithDiff :: (DBI repr, Weight repr w) => repr h (w -> Diff w w)
selfWithDiff = withDiff1 id

withDiff1 = app withDiff

class RandRange w where
  randRange :: (P.Double, P.Double) -> (w, w)

instance RandRange () where
  randRange _ = ((), ())

instance RandRange P.Double where
  randRange (lo, hi) = (lo, hi)

instance (RandRange l, RandRange r) => RandRange (l, r) where
  randRange (lo, hi) = ((llo, rlo), (lhi, rhi))
    where
      (llo, lhi) = randRange (lo, hi)
      (rlo, rhi) = randRange (lo, hi)

instance DBI repr => Weight repr () where
  withDiff = const1 id
  fromDiff _ = id

instance DBI repr => Weight repr P.Double where
  withDiff = hlam2 $ \conv d -> mkProd2 d (app conv litOne)
  fromDiff _ = zro

instance (DBI repr, Weight repr l, Weight repr r) => Weight repr (l, r) where
  withDiff = hlam $ \conv -> bimap2 (withDiff1 (hlam $ \l -> app conv (mkProd2 l zero))) (withDiff1 (hlam $ \r -> app conv (mkProd2 zero r)))
  fromDiff p = bimap2 (fromDiff p) (fromDiff p)

class (Random w, RandRange w, Reify repr w, P.Show w, Vector repr w) => Weight repr w where
  withDiff :: repr h ((w -> x) -> w -> Diff x w)
  fromDiff :: Proxy x -> repr h (Diff x w -> w)

data RunImpW repr h x = forall w. Weight repr w => RunImpW (repr h (w -> x))
data ImpW repr h x = NoImpW (repr h x) | forall w. Weight repr w => ImpW (repr h (w -> x))

runImpW :: forall repr h x. DBI repr => ImpW repr h x -> RunImpW repr h x
runImpW (ImpW x) = RunImpW x
runImpW (NoImpW x) = RunImpW (const1 x :: repr h (() -> x))

data Term con h x = Term (forall r. con r => r h x)

instance DBI repr => DBI (ImpW repr) where
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
  doublePlus = NoImpW doublePlus
  doubleMinus = NoImpW doubleMinus
  doubleMult = NoImpW doubleMult
  doubleDivide = NoImpW doubleDivide
  fix = NoImpW fix
  left = NoImpW left
  right = NoImpW right
  sumMatch = NoImpW sumMatch
  lit = NoImpW . lit
  writer = NoImpW writer
  runWriter = NoImpW runWriter
  z = NoImpW z
  s :: forall a h b. ImpW repr h b -> ImpW repr (a, h) b
  s (ImpW x) = work x
    where
      work :: Weight repr w => repr h (w -> b) -> ImpW repr (a, h) b
      work x = ImpW (s x)
  s (NoImpW x) = NoImpW (s x)
  app (ImpW f) (ImpW x) = ImpW (hlam $ \p -> app (app (conv f) (zro1 p)) (app (conv x) (fst1 p)))
  app (NoImpW f) (NoImpW x) = NoImpW (app f x)
  app (ImpW f) (NoImpW x) = ImpW (hlam $ \w -> app2 (conv f) w (conv x))
  app (NoImpW f) (ImpW x) = ImpW (hlam $ \w -> app (conv f) (app (conv x) w))
  lam (ImpW f) = ImpW (flip1 $ lam f)
  lam (NoImpW x) = NoImpW (lam x)
  exp = NoImpW exp

instance DBI (Term DBI) where
  z = Term z
  s (Term x) = Term (s x)
  lam (Term x) = Term (lam x)
  app (Term f) (Term x) = Term $ app f x
  mkProd = Term mkProd
  zro = Term zro
  fst = Term fst
  lit x = Term $ lit x
  doublePlus = Term doublePlus
  doubleMinus = Term doubleMinus
  doubleMult = Term doubleMult
  doubleDivide = Term doubleDivide
  fix = Term fix
  left = Term left
  right = Term right
  sumMatch = Term sumMatch
  unit = Term unit
  exfalso = Term exfalso
  nothing = Term nothing
  just = Term just
  optionMatch = Term optionMatch
  exp = Term exp
  ioRet = Term ioRet
  ioMap = Term ioMap
  ioBind = Term ioBind
  nil = Term nil
  cons = Term cons
  listMatch = Term listMatch
  writer = Term writer
  runWriter = Term runWriter