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
  PartialTypeSignatures,
  NoImplicitPrelude
#-}

module DDF.DBI (module DDF.DBI, module DDF.ImportMeta) where
import DDF.ImportMeta

class Monoid r m where
  zero :: r h m
  plus :: r h (m -> m -> m)

class DBI (r :: * -> * -> *) where
  z :: r (a, h) a
  s :: r h b -> r (a, h) b
  abs :: r (a, h) b -> r h (a -> b)
  app :: r h (a -> b) -> r h a -> r h b
  -- | We use a variant of HOAS so it can be compile to DBI, which is more compositional (No Negative Occurence).
  -- It require explicit lifting of variables.
  -- Use lam to do automatic lifting of variables.
  hoas :: (r (a, h) a -> r (a, h) b) -> r h (a -> b)
  hoas f = abs $ f z
  com :: r h ((b -> c) -> (a -> b) -> (a -> c))
  com = lam3 $ \f g x -> app f (app g x)
  flip :: r h ((a -> b -> c) -> (b -> a -> c))
  flip = lam3 $ \f b a -> app2 f a b
  id :: r h (a -> a)
  id = lam $ \x -> x
  const :: r h (a -> b -> a)
  const = lam2 $ \x _ -> x
  scomb :: r h ((a -> b -> c) -> (a -> b) -> (a -> c))
  scomb = lam3 $ \f x arg -> app2 f arg (app x arg)
  dup :: r h ((a -> a -> b) -> (a -> b))
  dup = lam2 $ \f x -> app2 f x x
  let_ :: r h (a -> (a -> b) -> b)
  let_ = flip1 id

class LiftEnv r where
  liftEnv :: r () a -> r h a

const1 = app const
map2 = app2 map
return = pure
bind2 = app2 bind
map1 = app map
join1 = app join
bimap2 = app2 bimap
bimap3 = app3 bimap
flip1 = app flip
flip2 = app2 flip
let_2 = app2 let_

class DBI r => Functor r f where
  map ::  r h ((a -> b) -> (f a -> f b))

class Functor r a => Applicative r a where
  pure :: r h (x -> a x)
  ap :: r h (a (x -> y) -> a x -> a y)
pure1 = app1 pure
ap1 = app1 ap
ap2 = app2 ap

class Applicative r m => Monad r m where
  bind :: r h (m a -> (a -> m b) -> m b)
  join :: r h (m (m a) -> m a)
  join = lam $ \m -> bind2 m id
  bind = lam2 $ \m f -> join1 (app2 map f m)
  {-# MINIMAL (join | bind) #-}

class BiFunctor r p where
  bimap :: r h ((a -> b) -> (c -> d) -> p a c -> p b d)

com2 = app2 com

class NT repr l r where
    conv :: repr l t -> repr r t

class NTS repr l r where
    convS :: repr l t -> repr r t

instance (DBI repr, NT repr l r) => NTS repr l (a, r) where
    convS = s . conv

instance {-# OVERLAPPABLE #-} NTS repr l r => NT repr l r where
    conv = convS

instance {-# OVERLAPPING #-} NT repr x x where
    conv x = x

lam :: forall repr a b h. DBI repr =>
  ((forall k. NT repr (a, h) k => repr k a) -> (repr (a, h)) b) ->
  repr h (a -> b)
lam f = hoas (\x -> f $ conv x)

lam2 :: forall repr a b c h. DBI repr =>
  ((forall k. NT repr (a, h) k => repr k a) ->
   (forall k. NT repr (b, (a, h)) k => repr k b) ->
   (repr (b, (a, h))) c) ->
  repr h (a -> b -> c)
lam2 f = lam $ \x -> lam $ \y -> f x y

lam3 f = lam2 $ \a b -> lam $ \c -> f a b c

lam4 f = lam3 $ \a b c -> lam $ \d -> f a b c d

app1 = app 

app2 f a = app (app1 f a)

app3 f a b = app (app2 f a b)

app4 f a b c = app (app3 f a b c)

app5 f a b c d = app (app4 f a b c d)

plus2 = app2 plus

noEnv :: repr () x -> repr () x
noEnv x = x

scomb2 = app2 scomb
plus1 = app plus
dup1 = app dup
