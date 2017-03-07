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
    NoMonomorphismRestriction #-}

module DBI where
import Util

class DBI repr where
  z :: repr (a, h) a
  s :: repr h b -> repr (a, h) b
  lam :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b
  mkprod :: repr h (a -> b -> (a, b))
  prodZro :: repr h ((a, b) -> a)
  prodFst :: repr h ((a, b) -> b)
  hoas :: (repr (a, h) a -> repr (a, h) b) -> repr h (a -> b)
  hoas f = lam $ f z

newtype Eval h x = Eval {unEval :: h -> x}

comb = Eval . const

instance DBI Eval where
  z = Eval fst
  s (Eval a) = Eval $ a . snd
  lam (Eval f) = Eval $ \a h -> f (h, a)
  app (Eval f) (Eval x) = Eval $ \h -> f h $ x h
  prodZro = comb fst
  prodFst = comb snd
  mkprod = comb (,)

newtype DShow h a = DShow {unDShow :: [String] -> Int -> String}

instance DBI DShow where
  z = DShow $ const $ show . flip (-) 1
  s (DShow v) = DShow $ \vars -> v vars . flip (-) 1
  lam (DShow f) = DShow $ \vars x -> "(\\" ++ show x ++ " -> " ++ f vars (x + 1) ++ ")"
  app (DShow f) (DShow x) = DShow $ \vars h -> "(" ++ f vars h ++ " " ++ x vars h ++ ")"
  hoas f = DShow $ \(v:vars) h ->
    "(\\" ++ v ++ " -> " ++ (unDShow $ f $ DShow $ const $ const v) vars h ++ ")"
  mkprod = DShow $ const $ const "mkprod"
  prodZro = DShow $ const $ const "zro"
  prodFst = DShow $ const $ const "fst"

class NT l r where
    conv :: l t -> r t

instance (DBI repr, NT (repr l) (repr r)) => NT (repr l) (repr (a, r)) where
    conv = s . conv

instance NT x x where
    conv = id

hlam :: forall repr a b h. DBI repr =>
 ((forall k. NT (repr (a, h)) k => k a) -> (repr (a, h)) b) -> repr h (a -> b)
hlam f = hoas (\x -> f $ conv x)

scomb = hlam $ \f -> hlam $ \x -> hlam $ \arg -> app (app f arg) (app x arg)

main :: IO ()
main = putStrLn $ unDShow scomb vars 0
