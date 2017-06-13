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

import DDF.DBI
import DDF.Double
import qualified Prelude as M

data P repr h a where
  Unk :: repr h a -> P repr h a
  Static :: a -> (forall h. repr h a) -> P repr h a
  StaFun :: (forall hout. EnvT repr (a, h) hout -> P repr hout b) ->
            P repr h (a -> b)
  Open   :: (forall hout. EnvT repr h hout -> P repr hout a) ->
            P repr h a

data EnvT repr hin hout where
  Dyn  :: EnvT repr hin hin
  Arg  :: P repr hout a -> EnvT repr (a, hout) hout
  Weak :: EnvT repr h (a, h)
  Next :: EnvT repr hin hout -> EnvT repr (a, hin) (a, hout)

dynamic:: DBI repr => P repr h a -> repr h a
dynamic (Unk x)      = x
dynamic (Static _ x) = x
dynamic (StaFun f)   = abs $ dynamic (f Dyn)
dynamic (Open f)     = dynamic (f Dyn)

app_open :: DBI repr =>
            P repr hin r -> EnvT repr hin hout -> P repr hout r
app_open e Dyn            = Unk (dynamic e)
            -- this was a constant function, ignore the argument
app_open (Static es ed) _ = Static es ed
app_open (Open fs) h      = fs h
app_open (StaFun fs) h    = abs (fs (Next h))
app_open (Unk e) h        = Unk (app_unk e h) where
  -- The reason we need the following
  --   consider the term (lam (z + dynamic z)) (int 1)
  -- The PE result should really be 1 + (lam z) (int 1)
  app_unk :: DBI repr =>
             repr hin a -> EnvT repr hin hout -> repr hout a
  app_unk e Dyn      = e
  app_unk e (Arg p)  = app (abs e) (dynamic p)
  app_unk e (Next h) = app (s (app_unk (abs e) h)) z
  app_unk e Weak     = s e

instance DBI r => DBI (P r) where
  z = Open f where
    f :: EnvT r (a,h) hout -> P r hout a
    f Dyn       = Unk z                 -- turn to dynamic as requested
    f (Arg x)   = x                     -- substitution
    f (Next _)  = z                     -- not my level
    f Weak      = s z

  s :: forall h a any. P r h a -> P r (any, h) a
  s (Unk x) = Unk (s x)
  s (Static a ar) = Static a ar
  s (StaFun fs) = abs (fs (Next Weak))
  s p = Open f where
    f :: EnvT r (any, h) hout -> P r hout a
    -- Nothing is statically known, dynamize
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

binaryPE :: forall h a r.
            DBI r => (a -> a -> a) -> r h (a -> a -> a) ->
                     (forall h. P r h (a -> a -> a)) -> (forall h. a -> P r h a) ->
                     (a -> M.Bool) -> (a -> M.Bool) -> P r h (a -> a -> a)
binaryPE op opM opPM liftM isLeftUnit isRightUnit = StaFun binaryPE'
  where
    binaryPE' :: forall hout. EnvT r (a, h) hout -> P r hout (a -> a)
    binaryPE' Dyn      = app opPM z
    binaryPE' (Arg a)  = StaFun (binaryPE'' a)
    binaryPE' (Next _) = app opPM z
    binaryPE' Weak     = app opPM (s z)

    binaryPE'' :: forall hout. P r h a -> EnvT r (a, h) hout -> P r hout a
    binaryPE'' a Dyn       = app2 opPM (s a) z
    binaryPE'' a (Arg b)   = f a b
    binaryPE'' a (Next h') = app2 opPM (s (app_open a h')) z
    binaryPE'' a Weak      = app2 opPM (s (s a)) (s z)

    f (Static d1 _) (Static d2 _)      = liftM (d1 `op` d2)
    f (Static d1 _) x | isLeftUnit d1  = x
    f x (Static d2 _) | isRightUnit d2 = x
    f (Unk x) (Unk y)                  = Unk (app2 opM x y)
    f e1 e2                            = Open (\h -> app2 opPM (app_open e1 h) (app_open e2 h))

unaryPE :: forall h a r.
           DBI r => (a -> a) -> r h (a -> a) ->
                    (forall h. P r h (a -> a)) -> (forall h. a -> P r h a) ->
                    P r h (a -> a)
unaryPE op opM opPM liftM = StaFun unaryPE'
  where
    unaryPE' :: forall hout. EnvT r (a, h) hout -> P r hout a
    unaryPE' Dyn      = app opPM z
    unaryPE' (Arg d)  = f d
    unaryPE' (Next _) = app opPM z
    unaryPE' Weak     = app opPM (s z)
    
    f (Static d _) = liftM (op d)
    f (Unk x) = Unk (app opM x)
    f e = Open (\h -> app opPM (app_open e h))

instance Double r => Double (P r) where
  double x = Static x (double x)
  doublePlus = binaryPE (+) doublePlus doublePlus double (== 0.0) (== 0.0)
  doubleMinus = binaryPE (-) doubleMinus doubleMinus double (M.const M.False) (== 0.0)
  doubleMult = binaryPE (*) doubleMult doubleMult double (== 1.0) (== 1.0)
  doubleDivide = binaryPE (/) doubleDivide doubleDivide double (M.const M.False) (== 1.0)
  doubleExp = unaryPE (M.exp) doubleExp doubleExp double

pe :: Double repr => P repr () a -> repr () a
pe = dynamic
