{-# LANGUAGE
  RankNTypes,
  NoImplicitPrelude,
  TypeOperators,
  NoMonomorphismRestriction
#-}

module DDF.DLang (module DDF.DLang, module DDF.Lang) where

import DDF.Lang
import qualified Prelude as M

class Lang r => DLang r where
  nextDiff :: (DiffVector r v, RTDiff r x, RTDiff r v) => Proxy v -> r h (InfDiff Eval () x -> InfDiff Eval () (Diff v x))
  infDiffGet :: RTDiff r x => r h (InfDiff Eval () x -> x)
  rtDiffDiff :: forall v x. (DiffVector r v, RTDiff r v) => Proxy r -> Proxy (v, x) -> RTDiff r x :- RTDiff r (Diff v x)
  intDLang :: Proxy r -> Dict (DLang (DiffInt r))
  litInfDiff :: DiffInt r () x -> r h (InfDiff Eval () x)
  infDiffApp :: r h (InfDiff Eval () (a -> b) -> InfDiff Eval () a -> InfDiff Eval () b)
  infDiffAppApp :: r h (InfDiff Eval () (a -> b -> c) -> InfDiff Eval () a -> InfDiff Eval () b -> InfDiff Eval () c)
  infDiffAppApp = lam2 $ \f x -> infDiffApp1 $ infDiffApp2 f x
  rtdd :: Proxy r -> Dict (RTDiff r M.Double)

infDiffApp1 = app infDiffApp
infDiffApp2 = app2 infDiffApp
infDiffAppApp1 = app infDiffAppApp
nextDiff1 p = app $ nextDiff p