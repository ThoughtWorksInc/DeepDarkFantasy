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
    ConstraintKinds,
    DefaultSignatures,
    TypeOperators,
    TypeApplications,
    PartialTypeSignatures #-}

module DBI where
import qualified Prelude as P
import Prelude (($), (.), (+), (-), (++), show, (>>=), (*), (/), undefined)
import Util
import Control.Monad (when)
import System.Random
import Data.Proxy
import Data.Constraint
import Data.Constraint.Forall

class Monoid r m where
  zero :: r h m
  plus :: r h (m -> m -> m)

class Monoid repr w => WithDiff repr w where
  withDiff :: repr h ((w -> x) -> w -> Diff x w)

class DBI repr => ConvDiff repr where
  toDiff :: forall h x w. Monoid repr x => Proxy x -> repr h (w -> Diff x w)
  toDiff _ = toDiffBy1 @repr @h @x @w zero
  toDiffBy :: forall h x w. repr h (x -> w -> Diff x w)
  fromDiff :: forall h x w. Monoid repr x => Proxy x -> repr h (Diff x w -> w)
  fromDiff _ = fromDiffBy1 @repr @h @x @w zero
  fromDiffBy :: repr h (x -> Diff x w -> w)

withDiff1 = app withDiff
toDiffBy1 :: forall repr h x w. ConvDiff repr => repr h x -> repr h (w -> Diff x w)
toDiffBy1 = app toDiffBy
fromDiffBy1 :: forall repr h x w. ConvDiff repr => repr h x -> repr h (Diff x w -> w)
fromDiffBy1 = app fromDiffBy

selfWithDiff :: (DBI repr, WithDiff repr w) => repr h (w -> Diff w w)
selfWithDiff = withDiff1 id

class DBI (repr :: * -> * -> *) where
  z :: repr (a, h) a
  s :: repr h b -> repr (a, h) b
  abs :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b
  hoas :: (repr (a, h) a -> repr (a, h) b) -> repr h (a -> b)
  hoas f = abs $ f z
  com :: repr h ((b -> c) -> (a -> b) -> (a -> c))
  com = lam3 $ \f g x -> app f (app g x)
  flip :: repr h ((a -> b -> c) -> (b -> a -> c))
  flip = lam3 $ \f b a -> app2 f a b
  id :: repr h (a -> a)
  id = lam $ \x -> x
  const :: repr h (a -> b -> a)
  const = lam2 $ \x _ -> x
  scomb :: repr h ((a -> b -> c) -> (a -> b) -> (a -> c))
  scomb = lam3 $ \f x arg -> app2 f arg (app x arg)
  dup :: repr h ((a -> a -> b) -> (a -> b))
  dup = lam2 $ \f x -> app2 f x x
  let_ :: repr h (a -> (a -> b) -> b)
  let_ = flip1 id

const1 = app const
map2 = app2 map
return = pure
bind2 = app2 bind
map1 = app map
join1 = app join
bimap2 = app2 bimap
flip1 = app flip
flip2 = app2 flip

class Functor r f where
  map :: r h ((a -> b) -> (f a -> f b))

class Functor r a => Applicative r a where
  pure :: r h (x -> a x)
  ap :: r h (a (x -> y) -> a x -> a y)

class (DBI r, Applicative r m) => Monad r m where
  bind :: r h (m a -> (a -> m b) -> m b)
  join :: r h (m (m a) -> m a)
  join = lam $ \m -> bind2 m id
  bind = lam2 $ \m f -> join1 (app2 map f m)
  {-# MINIMAL (join | bind) #-}

class BiFunctor r p where
  bimap :: r h ((a -> b) -> (c -> d) -> p a c -> p b d)

app3 f x y z = app (app2 f x y) z
com2 = app2 com

newtype Eval h x = Eval {runEval :: h -> x}

comb = Eval . P.const

instance DBI Eval where
  z = Eval P.fst
  s (Eval a) = Eval $ a . P.snd
  abs (Eval f) = Eval $ \a h -> f (h, a)
  app (Eval f) (Eval x) = Eval $ \h -> f h $ x h

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
  abs (Show f) = Show $ \vars x -> lamAST (show x) (f vars (x + 1))
  app (Show f) (Show x) = Show $ \vars h -> appAST (f vars h) (x vars h)
  hoas f = Show $ \(v:vars) h ->
    lamAST v (runShow (f $ Show $ P.const $ P.const $ Leaf v) vars (h + 1))

class NT repr l r where
    conv :: repr l t -> repr r t

class NTS repr l r where
    convS :: repr l t -> repr r t

instance (DBI repr, NT repr l r) => NTS repr l (a, r) where
    convS = s . conv

instance {-# OVERLAPPABLE #-} NTS repr l r => NT repr l r where
    conv = convS

instance {-# OVERLAPPING #-} NT repr x x where
    conv = P.id

lam :: forall repr a b h. DBI repr =>
  ((forall k. NT repr (a, h) k => repr k a) -> (repr (a, h)) b) -> repr h (a -> b)
lam f = hoas (\x -> f $ conv x)

lam2 :: forall repr a b c h. DBI repr =>
  ((forall k. NT repr (a, h) k => repr k a) -> (forall k. NT repr (b, (a, h)) k => repr k b) -> (repr (b, (a, h))) c) -> repr h (a -> b -> c)
lam2 f = lam $ \x -> lam $ \y -> f x y

lam3 f = lam2 $ \x y -> lam $ \z -> f x y z

type family Diff v x
type instance Diff v () = ()
type instance Diff v (a -> b) = Diff v a -> Diff v b
type instance Diff v (a, b) = (Diff v a, Diff v b)

newtype WDiff repr v h x = WDiff {runWDiff :: repr (Diff v h) (Diff v x)}

app2 f a = app (app f a)

plus2 = app2 plus

instance DBI repr => DBI (WDiff repr v) where
  z = WDiff z
  s (WDiff x) = WDiff $ s x
  abs (WDiff f) = WDiff $ abs f
  app (WDiff f) (WDiff x) = WDiff $ app f x
  hoas f = WDiff $ hoas (runWDiff . f . WDiff)

noEnv :: repr () x -> repr () x
noEnv = P.id

instance Weight () where weightCon = Sub Dict

instance Weight P.Double where weightCon = Sub Dict

instance (Weight l, Weight r) => Weight (l, r) where
  weightCon :: forall con. (con (), con P.Float, con P.Double, ForallV (ProdCon con)) :- con (l, r)
  weightCon = Sub (mapDict (prodCon \\ (instV :: (ForallV (ProdCon con) :- ProdCon con l r))) (Dict \\ weightCon @l @con \\ weightCon @r @con))

class ProdCon con l r where
  prodCon :: (con l, con r) :- con (l, r)

instance ProdCon Random l r where prodCon = Sub Dict

instance ProdCon RandRange l r where prodCon = Sub Dict

instance ProdCon P.Show l r where prodCon = Sub Dict

class Weight w where
  weightCon :: (con (), con P.Float, con P.Double, ForallV (ProdCon con)) :- con w

data RunImpW repr h x = forall w. Weight w => RunImpW (repr h (w -> x))
data ImpW repr h x = NoImpW (repr h x) | forall w. Weight w => ImpW (repr h (w -> x))

type RunImpWR repr h x = forall r. (forall w. Weight w => repr h (w -> x) -> r) -> r

runImpW2RunImpWR :: RunImpW repr h x -> RunImpWR repr h x
runImpW2RunImpWR (RunImpW x) = \f -> f x

runImpWR2RunImpW :: RunImpWR repr h x -> RunImpW repr h x
runImpWR2RunImpW f = f RunImpW

data Combine l r h x = Combine (l h x) (r h x)

instance (DBI l, DBI r) => DBI (Combine l r) where
  z = Combine z z
  s (Combine l r) = Combine (s l) (s r)
  app (Combine fl fr) (Combine xl xr) = Combine (app fl xl) (app fr xr)
  abs (Combine l r) = Combine (abs l) (abs r)
  hoas f = Combine (hoas $ \x -> case f (Combine x z) of Combine l r -> l) (hoas $ \x -> case f (Combine z x) of Combine l r -> r)
