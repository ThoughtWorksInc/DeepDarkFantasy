{-# LANGUAGE
  RankNTypes,
  NoImplicitPrelude,
  GADTs,
  ExplicitForAll,
  ScopedTypeVariables,
  NoMonomorphismRestriction,
  IncoherentInstances,
  InstanceSigs,
  LambdaCase,
  FlexibleContexts,
  KindSignatures,
  TypeFamilies
#-}

module DDF.PE where

import DDF.Lang
import qualified Prelude as M

data P repr h a where
  Open   :: (forall hout. EnvT repr h hout -> P repr hout a) -> P repr h a
  Unk    :: repr h a -> P repr h a
  Known  ::
    K repr h a ->
    repr h a ->
    (forall hout. EnvT repr h hout -> P repr hout a) ->
    (forall any. P repr (any, h) a) ->
    (forall hh ht. (hh, ht) ~ h => P repr ht (hh -> a)) ->
    P repr h a

isOpen (Open _) = M.True
isOpen _ = M.False
type family   K (repr :: * -> * -> *) h a
type instance K repr h (a, b)   = (P repr h a, P repr h b)
type instance K repr h M.Bool   = M.Bool
type instance K repr h M.Double = M.Double
type instance K repr h (a -> b) = Fun repr h a b
newtype Fun repr h a b = Fun {runFun :: forall hout. EnvT repr (a, h) hout -> P repr hout b}

mkFun :: Prod repr => (forall hout. EnvT repr (a, h) hout -> P repr hout b) -> P repr h (a -> b)
mkFun f = Known (Fun f) (abs $ dynamic (f Dyn)) (\h -> abs $ f $ Next h) (abs $ f $ Next Weak) (mkFun $ app_open (mkFun f))

data EnvT repr hin hout where
  Dyn  :: EnvT repr hin hin
  Arg  :: P repr hout a -> EnvT repr (a, hout) hout
  Weak :: EnvT repr h (a, h)
  Next :: EnvT repr hin hout -> EnvT repr (a, hin) (a, hout)

dynamic:: Prod repr => P repr h a -> repr h a
dynamic (Unk x)      = x
dynamic (Open f)     = dynamic (f Dyn)
dynamic (Known _ d _ _ _)  = d

app_open :: Prod repr => P repr hin r -> EnvT repr hin hout -> P repr hout r
app_open (Open fs) h       = fs h
app_open (Unk e) Dyn       = Unk e
app_open (Unk e) (Arg p)   = Unk (app (abs e) (dynamic p))
app_open (Unk e) (Next h)  = app (s (app_open (Unk (abs e)) h)) z
app_open (Unk e) Weak      = Unk (s e)
app_open (Known _ _ x _ _) h = x h

instance Prod r => DBI (P r) where
  z = Open f where
    f :: EnvT r (a,h) hout -> P r hout a
    f Dyn      = Unk z
    f (Arg x)  = x
    f (Next _) = z
    f Weak     = s z

  s :: forall h a any. P r h a -> P r (any, h) a
  s (Unk x) = Unk (s x)
  s (Known _ _ _ x _) = x
  s p@(Open _) = Open f where
    f :: EnvT r (any, h) hout -> P r hout a
    f Dyn              = Unk (s (dynamic p))
    f (Arg _)          = p
    f (Next h)         = s (app_open p h)
    f Weak             = s (s p)

  abs (Unk f) = Unk (abs f)
  abs o@(Open _) = mkFun (app_open o)
  abs (Known _ _ _ _ x) = x

  app (Known (Fun fs) _ _ _ _) p     = fs (Arg p)
  app e1 e2 | isOpen e1 || isOpen e2 = Open (\h -> app (app_open e1 h) (app_open e2 h))
  app f x                            = Unk (app (dynamic f) (dynamic x))

instance (Prod r, Bool r) => Bool (P r) where
  bool x = Known x (bool x) (\_ -> bool x) (bool x) (mkFun (\_ -> bool x))
  ite = lam3 (\l r b -> app2 (f b) l r)
    where
      f :: P r h M.Bool -> P r h (a -> a -> a)
      f (Known M.True _ _ _ _) = const
      f (Known M.False _ _ _ _) = const1 id
      f (Unk x) = Unk (lam2 (\l r -> ite3 l r (s (s x))))
      f x@(Open _) = Open (\h -> f (app_open x h))

instance (Prod r, Double r) => Double (P r) where
  double x = Known x (double x) (\_ -> double x) (double x) (mkFun (\_ -> double x))
  doublePlus = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Known l _ _ _ _) (Known r _ _ _ _) = double (l + r)
      f (Known 0 _ _ _ _) r = r
      f l (Known 0 _ _ _ _) = l
      f l r | isOpen l || isOpen r = Open (\h -> f (app_open l h) (app_open r h))
      f l r = Unk (doublePlus2 (dynamic l) (dynamic r))
  doubleMult = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Known l _ _ _ _) (Known r _ _ _ _) = double (l * r)
      f (Known 0 _ _ _ _) _ = double 0
      f _ (Known 0 _ _ _ _) = double 0
      f l (Known 1 _ _ _ _) = l
      f (Known 1 _ _ _ _) r = r
      f l r | isOpen l || isOpen r = Open (\h -> f (app_open l h) (app_open r h))
      f l r = Unk (doubleMult2 (dynamic l) (dynamic r))
  doubleMinus = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Known l _ _ _ _) (Known r _ _ _ _) = double (l - r)
      f l (Known 0 _ _ _ _) = l 
      f l r | isOpen l || isOpen r = Open (\h -> f (app_open l h) (app_open r h))
      f l r = Unk (doubleMinus2 (dynamic l) (dynamic r))
  doubleDivide = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Known l _ _ _ _) (Known r _ _ _ _) = double (l / r)
      f (Known 0 _ _ _ _) _ = double 0
      f l (Known 1 _ _ _ _) = l 
      f l r | isOpen l || isOpen r = Open (\h -> f (app_open l h) (app_open r h))
      f l r = Unk (doubleDivide2 (dynamic l) (dynamic r))
  doubleExp = abs (f z)
    where
      f :: P r h M.Double -> P r h M.Double
      f (Known l _ _ _ _) = double (M.exp l) 
      f (Unk l) = Unk (doubleExp1 l)
      f l@(Open _) = Open (\h -> f (app_open l h))
  doubleEq = abs (abs (f (s z) z)) where
    f :: P r h M.Double -> P r h M.Double -> P r h M.Bool
    f (Known l _ _ _ _) (Known r _ _ _ _) = bool (l == r)
    f l r | isOpen l || isOpen r = Open (\h -> f (app_open l h) (app_open r h))
    f l r = Unk (doubleEq2 (dynamic l) (dynamic r))

instance Prod r => Prod (P r) where
  mkProd = abs (abs (f (s z) z))
    where
      f :: P r h a -> P r h b -> P r h (a, b)
      f l r = Known (l, r)
                (mkProd2 (dynamic l) (dynamic r))
                (\h -> mkProd2 (app_open l h) (app_open r h))
                (s (mkProd2 l r))
                (mkFun $ \x -> mkProd2 (app_open l x) (app_open r x))
  zro = abs (f z)
    where
      f :: P r h (a, b) -> P r h a
      f (Known (l, _) _ _ _ _) = l
      f (Unk p) = Unk (zro1 p)
      f p = Open (\h -> f (app_open p h))
  fst = abs (f z)
    where
      f :: P r h (a, b) -> P r h b
      f (Known (_, r) _ _ _ _) = r
      f (Unk p) = Unk (fst1 p)
      f p = Open (\h -> f (app_open p h))

pe :: Prod repr => P repr () a -> repr () a
pe = dynamic
