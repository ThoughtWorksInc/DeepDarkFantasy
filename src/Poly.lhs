> {-# LANGUAGE
>     MultiParamTypeClasses,
>     RankNTypes,
>     ScopedTypeVariables,
>     FlexibleInstances,
>     FlexibleContexts,
>     UndecidableInstances,
>     IncoherentInstances,
>     PolyKinds,
>     LambdaCase,
>     NoMonomorphismRestriction,
>     TypeFamilies,
>     LiberalTypeSynonyms,
>     EmptyCase #-}

> module Poly where
> import Control.Monad (when)
> import Util
> import DBI hiding (main)

> test :: NT (repr (Double, h)) (repr (Double, h)) => Double
> test = 1
> meow :: Double
> meow = test
> poly :: forall repr h. DBI repr => repr h (Double -> Double)
> poly = hlam $ \x -> x
> l2 = hlam $ \x -> mult2 (minus2 x (lit 27)) (minus2 x (lit 27))
> --comp = hlam $ \x -> app l2 (app poly x)

> isSquare n = sq * sq == n
>   where sq = floor $ sqrt (fromIntegral n::Double)

> main :: IO ()
> main = do
>   print $ unDShow scomb vars 0
>   --print $ unDShow poly vars 0
>   go 0 0
>   where
>     go :: Integer -> Double -> IO ()
>     --go i w | i < 200 = do
>     --  when (isSquare i) $ print w
>     --  go (1 + i) $ w - 0.001 * snd (unEval (unWDiff $ noEnv comp) () (w, 1))
>     go i w = return ()
