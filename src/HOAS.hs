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

module HOAS where
import Util

class HOAS repr where
  app :: repr (a -> b) -> repr a -> repr b
  lam :: (repr a -> repr b) -> repr (a -> b)

newtype Eval x = Eval {unEval :: x}

instance HOAS Eval where
  app (Eval f) (Eval x) = Eval (f x)
  lam f = Eval (unEval . f . Eval)

newtype HShow x = HShow {unHShow :: [String] -> String}

instance HOAS HShow where
  app (HShow f) (HShow x) = HShow (\vars -> "(" ++ f vars ++ " " ++ x vars ++ ")")
  lam f = HShow (\(v:vars) -> "(\\" ++ v ++ " -> " ++ (unHShow $ f $ HShow $ const v) vars ++ ")")

s = lam (\f -> lam (\x -> lam (\arg -> app (app f arg) (app x arg))))

main :: IO ()
main = putStrLn ((unHShow s) $ vars)
