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
    AllowAmbiguousTypes,
    ExistentialQuantification #-}

module DBI where
import Util
import Data.Void
import Control.Monad (when)

class DBI repr where
  z :: repr (a, h) a
  s :: repr h b -> repr (a, h) b
  lam :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b
  mkprod :: repr h (a -> b -> (a, b))
  prodZro :: repr h ((a, b) -> a)
  prodFst :: repr h ((a, b) -> b)
  lit :: Double -> repr h Double
  litZro :: repr h Double
  litZro = lit 0
  litFst :: repr h Double
  litFst = lit 1
  plus :: repr h (Double -> Double -> Double)
  minus :: repr h (Double -> Double -> Double)
  mult :: repr h (Double -> Double -> Double)
  divide :: repr h (Double -> Double -> Double)
  hoas :: (repr (a, h) a -> repr (a, h) b) -> repr h (a -> b)
  hoas f = lam $ f z
  fix :: repr h ((a -> a) -> a)
  left :: repr h (a -> Either a b)
  right :: repr h (b -> Either a b)
  sumMatch :: repr h ((a -> c) -> (b -> c) -> Either a b -> c)
  unit :: repr h ()
  exfalso :: repr h (Void -> a)
  nothing :: repr h (Maybe a)
  just :: repr h (a -> Maybe a)
  optionMatch :: repr h (b -> (a -> b) -> Maybe a -> b)
  ioRet :: repr h (a -> IO a)
  ioBind :: repr h (IO a -> (a -> IO b) -> IO b)
  nil :: repr h [a]
  cons :: repr h (a -> [a] -> [a])
  listMatch :: repr h (b -> (a -> [a] -> b) -> [a] -> b)

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
  lit = comb
  plus = comb (+)
  minus = comb (-)
  mult = comb (*)
  divide = comb (/)
  fix = comb loop
    where loop x = x $ loop x
  left = comb Left
  right = comb Right
  sumMatch = comb $ \l r -> \case
                             Left x -> l x
                             Right x -> r x
  unit = comb ()
  exfalso = comb absurd
  nothing = comb Nothing
  just = comb Just
  ioRet = comb return
  ioBind = comb (>>=)
  nil = comb []
  cons = comb (:)
  listMatch = comb $ \l r -> \case
                            [] -> l
                            x:xs -> r x xs
  optionMatch = comb $ \l r -> \case
                              Nothing -> l
                              Just x -> r x

data AST = Leaf String | App String AST [AST] | Lam String [String] AST

appAST (Leaf f) x = App f x []
appAST (App f x l) r = App f x (l ++ [r])
appAST lam r = appAST (Leaf $ show lam) r

lamAST str (Lam s l t) = Lam str (s:l) t
lamAST str r = Lam str [] r

instance Show AST where
  show (Leaf f) = f
  show (App f x l) = "(" ++ f ++ " " ++ show x ++ concatMap ((" " ++) . show) l ++ ")"
  show (Lam s l t) = "(\\" ++ s ++ concatMap (" " ++) l ++ " -> " ++ show t ++ ")"
newtype DShow h a = DShow {unDShow :: [String] -> Int -> AST}
name = DShow . const . const . Leaf

instance DBI DShow where
  z = DShow $ const $ Leaf . show . flip (-) 1
  s (DShow v) = DShow $ \vars -> v vars . flip (-) 1
  lam (DShow f) = DShow $ \vars x -> lamAST (show x) (f vars (x + 1))
  app (DShow f) (DShow x) = DShow $ \vars h -> appAST (f vars h) (x vars h)
  hoas f = DShow $ \(v:vars) h ->
    lamAST v (unDShow (f $ DShow $ const $ const $ Leaf v) vars h)
  mkprod = name "mkprod"
  prodZro = name "zro"
  prodFst = name "fst"
  lit x = name $ show x
  plus = name "plus"
  minus = name "minus"
  mult = name "mult"
  divide = name "divide"
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

class NT repr l r where
    conv :: repr l t -> repr r t

instance {-# INCOHERENT #-} (DBI repr, NT repr l r) => NT repr l (a, r) where
    conv = s . conv

instance NT repr x x where
    conv = id

hlam :: forall repr a b h. DBI repr =>
 ((forall k. NT repr ((a, h)) k => repr k a) -> (repr (a, h)) b) -> repr h (a -> b)
hlam f = hoas (\x -> f $ conv x)

type family Diff x
type instance Diff Double = (Double, Double)
type instance Diff () = ()
type instance Diff (a, b) = (Diff a, Diff b)
type instance Diff (a -> b) = Diff a -> Diff b
type instance Diff (Either a b) = Either (Diff a) (Diff b)
type instance Diff Void = Void
type instance Diff (Maybe a) = Maybe (Diff a)
type instance Diff (IO a) = IO (Diff a)
type instance Diff [a] = [Diff a]

newtype WDiff repr h x = WDiff {unWDiff :: repr (Diff h) (Diff x)}

app2 f a = app (app f a)

mkprod2 = app2 mkprod
plus2 = app2 plus
prodZro1 = app prodZro
prodFst1 = app prodFst
minus2 = app2 minus
mult2 = app2 mult
divide2 = app2 divide

instance DBI repr => DBI (WDiff repr) where
  z = WDiff z
  s (WDiff x) = WDiff $ s x
  lam (WDiff f) = WDiff $ lam f
  app (WDiff f) (WDiff x) = WDiff $ app f x
  mkprod = WDiff mkprod
  prodZro = WDiff prodZro
  prodFst = WDiff prodFst
  lit x = WDiff $ app (app mkprod (lit x)) (lit 0)
  plus = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (plus2 (prodZro1 l) (prodZro1 r)) (plus2 (prodFst1 l) (prodFst1 r))
  minus = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (minus2 (prodZro1 l) (prodZro1 r)) (minus2 (prodFst1 l) (prodFst1 r))
  mult = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (mult2 (prodZro1 l) (prodZro1 r))
      (plus2 (mult2 (prodZro1 l) (prodFst1 r)) (mult2 (prodZro1 r) (prodFst1 l)))
  divide = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (divide2 (prodZro1 l) (prodZro1 r))
      (divide2 (minus2 (mult2 (prodZro1 r) (prodFst1 l)) (mult2 (prodZro1 l) (prodFst1 r)))
        (mult2 (prodZro1 r) (prodZro1 r)))
  hoas f = WDiff $ hoas (unWDiff . f . WDiff)
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

scomb = hlam $ \f -> hlam $ \x -> hlam $ \arg -> app (app f arg) (app x arg)

noEnv :: repr () x -> repr () x
noEnv = id
