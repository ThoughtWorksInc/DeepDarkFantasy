{-# LANGUAGE
    MultiParamTypeClasses,
    RankNTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    FlexibleContexts,
    UndecidableInstances,
    IncoherentInstances,
    PolyKinds,
    LambdaCase,
    MonomorphismRestriction #-}

module Comb where

class Comb repr where
  app :: repr (a -> b) -> repr a -> repr b
  s :: repr ((a -> b -> c) -> (a -> b) -> (a -> c))
  k :: repr (a -> b -> a)
  i :: repr (a -> a)
  b :: repr ((b -> c) -> (a -> b) -> (a -> c))
  c :: repr ((a -> b -> c) -> (b -> a -> c))
  w :: repr ((a -> a -> b) -> (a -> b))

newtype Eval x = Eval {unEval :: x}

instance Comb Eval where
  app (Eval f) (Eval x) = Eval (f x)
  s = Eval (\f x arg -> f arg $ x arg)
  k = Eval const
  i = Eval id
  b = Eval (.)
  c = Eval flip
  w = Eval (\f x -> f x x)

newtype SShow x = SShow {unSShow :: String}

instance Comb SShow where
  app (SShow f) (SShow x) = SShow $ "(" ++ f ++ " " ++ x ++ ")"
  s = SShow "s"
  k = SShow "k"
  i = SShow "i"
  b = SShow "b"
  c = SShow "c"
  w = SShow "w"

main :: IO ()
main = return ()
