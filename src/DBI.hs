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
import qualified Prelude as P
import Prelude (($), (.), const, (+), flip, id, (-), (++), show, (>>=), (*), (/), undefined)
import Util
import Data.Void
import Control.Monad (when)

class DBI repr where
  z :: repr (a, h) a
  s :: repr h b -> repr (a, h) b
  lam :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b
  mkprod :: repr h (a -> b -> (a, b))
  zro :: repr h ((a, b) -> a)
  fst :: repr h ((a, b) -> b)
  lit :: P.Double -> repr h P.Double
  litZro :: repr h P.Double
  litZro = lit 0
  litOne :: repr h P.Double
  litOne = lit 1
  plus :: repr h (P.Double -> P.Double -> P.Double)
  minus :: repr h (P.Double -> P.Double -> P.Double)
  mult :: repr h (P.Double -> P.Double -> P.Double)
  divide :: repr h (P.Double -> P.Double -> P.Double)
  hoas :: (repr (a, h) a -> repr (a, h) b) -> repr h (a -> b)
  hoas f = lam $ f z
  fix :: repr h ((a -> a) -> a)
  left :: repr h (a -> P.Either a b)
  right :: repr h (b -> P.Either a b)
  sumMatch :: repr h ((a -> c) -> (b -> c) -> P.Either a b -> c)
  unit :: repr h ()
  exfalso :: repr h (Void -> a)
  nothing :: repr h (P.Maybe a)
  just :: repr h (a -> P.Maybe a)
  optionMatch :: repr h (b -> (a -> b) -> P.Maybe a -> b)
  ioRet :: repr h (a -> P.IO a)
  ioBind :: repr h (P.IO a -> (a -> P.IO b) -> P.IO b)
  ioMap :: repr h ((a -> b) -> P.IO a -> P.IO b)
  nil :: repr h [a]
  cons :: repr h (a -> [a] -> [a])
  listMatch :: repr h (b -> (a -> [a] -> b) -> [a] -> b)
  com :: repr h ((b -> c) -> (a -> b) -> (a -> c))
  com = hlam $ \f -> hlam $ \g -> hlam $ \x -> app f (app g x)

class Functor r f where
  map :: r h ((a -> b) -> (f a -> f b))

class Functor r m => Monad r m where
  return :: r h (a -> (m a))
  bind :: r h (m a -> (a -> m b) -> m b)

instance DBI r => Functor r P.IO where
  map = ioMap

instance DBI r => Monad r P.IO where
  return = ioRet
  bind = ioBind

app3 f x y z = app (app2 f x y) z

optionMatch3 = app3 optionMatch
optionMatch2 = app2 optionMatch
com2 = app2 com

instance DBI r => Functor r P.Maybe where
  map = hlam $ \func -> optionMatch2 nothing (com2 just func)

instance DBI r => Monad r P.Maybe where
  return = just
  bind = hlam $ \x -> hlam $ \func -> optionMatch3 nothing func x

newtype Eval h x = Eval {unEval :: h -> x}

comb = Eval . const

instance DBI Eval where
  z = Eval P.fst
  s (Eval a) = Eval $ a . P.snd
  lam (Eval f) = Eval $ \a h -> f (h, a)
  app (Eval f) (Eval x) = Eval $ \h -> f h $ x h
  zro = comb P.fst
  fst = comb P.snd
  mkprod = comb (,)
  lit = comb
  plus = comb (+)
  minus = comb (-)
  mult = comb (*)
  divide = comb (/)
  fix = comb loop
    where loop x = x $ loop x
  left = comb P.Left
  right = comb P.Right
  sumMatch = comb $ \l r -> \case
                             P.Left x -> l x
                             P.Right x -> r x
  unit = comb ()
  exfalso = comb absurd
  nothing = comb P.Nothing
  just = comb P.Just
  ioRet = comb P.return
  ioBind = comb (>>=)
  nil = comb []
  cons = comb (:)
  listMatch = comb $ \l r -> \case
                            [] -> l
                            x:xs -> r x xs
  optionMatch = comb $ \l r -> \case
                              P.Nothing -> l
                              P.Just x -> r x
  ioMap = comb P.fmap

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
newtype Show h a = Show {unShow :: [P.String] -> P.Int -> AST}
name = Show . const . const . Leaf

instance DBI Show where
  z = Show $ const $ Leaf . show . flip (-) 1
  s (Show v) = Show $ \vars -> v vars . flip (-) 1
  lam (Show f) = Show $ \vars x -> lamAST (show x) (f vars (x + 1))
  app (Show f) (Show x) = Show $ \vars h -> appAST (f vars h) (x vars h)
  hoas f = Show $ \(v:vars) h ->
    lamAST v (unShow (f $ Show $ const $ const $ Leaf v) vars h)
  mkprod = name "mkprod"
  zro = name "zro"
  fst = name "fst"
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
  ioMap = name "ioMap"

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
type instance Diff P.Double = (P.Double, P.Double)
type instance Diff () = ()
type instance Diff (a, b) = (Diff a, Diff b)
type instance Diff (a -> b) = Diff a -> Diff b
type instance Diff (P.Either a b) = P.Either (Diff a) (Diff b)
type instance Diff Void = Void
type instance Diff (P.Maybe a) = P.Maybe (Diff a)
type instance Diff (P.IO a) = P.IO (Diff a)
type instance Diff [a] = [Diff a]

newtype WDiff repr h x = WDiff {unWDiff :: repr (Diff h) (Diff x)}

app2 f a = app (app f a)

mkprod2 = app2 mkprod
plus2 = app2 plus
zro1 = app zro
fst1 = app fst
minus2 = app2 minus
mult2 = app2 mult
divide2 = app2 divide

instance DBI repr => DBI (WDiff repr) where
  z = WDiff z
  s (WDiff x) = WDiff $ s x
  lam (WDiff f) = WDiff $ lam f
  app (WDiff f) (WDiff x) = WDiff $ app f x
  mkprod = WDiff mkprod
  zro = WDiff zro
  fst = WDiff fst
  lit x = WDiff $ app (app mkprod (lit x)) (lit 0)
  plus = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (plus2 (zro1 l) (zro1 r)) (plus2 (fst1 l) (fst1 r))
  minus = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (minus2 (zro1 l) (zro1 r)) (minus2 (fst1 l) (fst1 r))
  mult = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (mult2 (zro1 l) (zro1 r))
      (plus2 (mult2 (zro1 l) (fst1 r)) (mult2 (zro1 r) (fst1 l)))
  divide = WDiff $ hlam $ \l -> hlam $ \r ->
    mkprod2 (divide2 (zro1 l) (zro1 r))
      (divide2 (minus2 (mult2 (zro1 r) (fst1 l)) (mult2 (zro1 l) (fst1 r)))
        (mult2 (zro1 r) (zro1 r)))
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
  ioMap = WDiff ioMap

scomb = hlam $ \f -> hlam $ \x -> hlam $ \arg -> app (app f arg) (app x arg)

noEnv :: repr () x -> repr () x
noEnv = id
