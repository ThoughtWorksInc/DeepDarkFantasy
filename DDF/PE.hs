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
  FlexibleContexts
#-}

module DDF.PE where

import DDF.Lang
import qualified Prelude as M

data P repr h a where
  Unk :: repr h a -> P repr h a
  Static :: a -> (forall h'. repr h' a) -> P repr h a
  Prod :: P repr h a -> P repr h b -> P repr h (a, b)
  StaFun :: (forall hout. EnvT repr (a, h) hout -> P repr hout b) -> P repr h (a -> b)
  Open   :: (forall hout. EnvT repr h hout -> P repr hout a) -> P repr h a

data EnvT repr hin hout where
  Dyn  :: EnvT repr hin hin
  Arg  :: P repr hout a -> EnvT repr (a, hout) hout
  Weak :: EnvT repr h (a, h)
  Next :: EnvT repr hin hout -> EnvT repr (a, hin) (a, hout)

dynamic:: Prod repr => P repr h a -> repr h a
dynamic (Unk x)      = x
dynamic (Static _ x) = x
dynamic (StaFun f)   = abs $ dynamic (f Dyn)
dynamic (Open f)     = dynamic (f Dyn)
dynamic (Prod l r)   = dynamic (mkProd2 l r)

app_open :: Prod repr => P repr hin r -> EnvT repr hin hout -> P repr hout r
app_open e Dyn            = Unk (dynamic e)
app_open (Static es ed) _ = Static es ed
app_open (Open fs) h      = fs h
app_open (StaFun fs) h    = abs (fs (Next h))
app_open (Unk env) h      = Unk (app_unk env h) where
  app_unk :: Prod repr => repr hin a -> EnvT repr hin hout -> repr hout a
  app_unk e Dyn      = e
  app_unk e (Arg p)  = app (abs e) (dynamic p)
  app_unk e (Next h') = app (s (app_unk (abs e) h')) z
  app_unk e Weak     = s e
app_open (Prod l r) h = Prod (app_open l h) (app_open r h)

instance Prod r => DBI (P r) where
  z = Open f where
    f :: EnvT r (a,h) hout -> P r hout a
    f Dyn       = Unk z
    f (Arg x)   = x
    f (Next _)  = z
    f Weak      = s z

  s :: forall h a any. P r h a -> P r (any, h) a
  s (Unk x) = Unk (s x)
  s (Static a ar) = Static a ar
  s (StaFun fs) = abs (fs (Next Weak))
  s p = Open f where
    f :: EnvT r (any, h) hout -> P r hout a
    f Dyn              = Unk (s (dynamic p))
    f (Arg _)          = p
    f (Next h)         = s (app_open p h)
    f Weak             = s (s p)

  abs (Unk f) = Unk (abs f)
  abs (Static k ks) = StaFun $ \_ -> Static k ks
  abs body = StaFun (app_open body)
  
  app (Unk f) (Unk x) = Unk (app f x)
  app (StaFun fs) p   = fs (Arg p)
  app (Static _ fs) p = Unk (app fs (dynamic p))
  app e1 e2           = Open (\h -> app (app_open e1 h) (app_open e2 h))

instance (Prod r, Bool r) => Bool (P r) where
  bool x = Static x (bool x)
  ite = lam3 (\l r b -> app2 (f b) l r)
    where
      f :: P r h M.Bool -> P r h (a -> a -> a)
      f (Static M.True _) = const
      f (Static M.False _) = const1 id
      f (Unk x) = Unk (lam2 (\l r -> ite3 l r (s (s x))))
      f x = Open (\h -> f (app_open x h))

instance (Prod r, Double r) => Double (P r) where
  double x = Static x (double x)
  doublePlus = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Static l _) (Static r _) = double (l + r)
      f (Static 0 _) r = r
      f l (Static 0 _) = l
      f (Unk l) (Unk r) = Unk (doublePlus2 l r)
      f l r = Open (\h -> f (app_open l h) (app_open r h))
  doubleMult = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Static l _) (Static r _) = double (l * r)
      f (Static 0 _) _ = double 0
      f _ (Static 0 _) = double 0
      f l (Static 1 _) = l
      f (Static 1 _) r = r
      f (Unk l) (Unk r) = Unk (doubleMult2 l r)
      f l r = Open (\h -> f (app_open l h) (app_open r h))
  doubleMinus = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Static l _) (Static r _) = double (l - r)
      f l (Static 0 _) = l
      f (Unk l) (Unk r) = Unk (doubleMinus2 l r)
      f l r = Open (\h -> f (app_open l h) (app_open r h))
  doubleDivide = abs (abs (f (s z) z))
    where
      f :: P r h M.Double -> P r h M.Double -> P r h M.Double
      f (Static l _) (Static r _) = double (l / r)
      f (Static 0 _) _ = double 0
      f l (Static 1 _) = l
      f (Unk l) (Unk r) = Unk (doubleDivide2 l r)
      f l r = Open (\h -> f (app_open l h) (app_open r h))
  doubleExp = abs (f z)
    where
      f :: P r h M.Double -> P r h M.Double
      f (Static l _) = double (M.exp l) 
      f (Unk l) = Unk (doubleExp1 l)
      f l = Open (\h -> f (app_open l h))
  doubleEq = abs (abs (f (s z) z)) where
    f :: P r h M.Double -> P r h M.Double -> P r h M.Bool
    f (Static l _) (Static r _) = bool (l == r)
    f (Unk l) (Unk r) = Unk (doubleEq2 l r)
    f l r = Open (\h -> f (app_open l h) (app_open r h))    

instance Prod r => Prod (P r) where
  mkProd = abs (abs (Prod (s z) z))
  zro = abs (f z)
    where
      f :: P r h (a, b) -> P r h a
      f (Static (l, _) p) = Static l (zro1 p)
      f (Prod l _) = l
      f (Unk p) = Unk (zro1 p)
      f p = Open (\h -> f (app_open p h))
  fst = abs (f z)
    where
      f :: P r h (a, b) -> P r h b
      f (Static (_, r) p) = Static r (fst1 p)
      f (Prod _ r) = r
      f (Unk p) = Unk (fst1 p)
      f p = Open (\h -> f (app_open p h))

pe :: Prod repr => P repr () a -> repr () a
pe = dynamic
