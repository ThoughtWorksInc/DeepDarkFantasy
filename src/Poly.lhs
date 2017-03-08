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
>     EmptyCase #-}

> module Poly where
> import Control.Monad (when)
> import Util
> import DBI

> main :: IO ()
> main = do
>   print $ unDShow scomb vars 0
>   print $ unDShow poly vars 0
>   go 0 0
>   where
>     poly = hlam $ \x -> plus2 (plus2 (mult2 x x) (mult2 (lit 2) x)) (lit 3)
>     l2 = hlam $ \x -> mult2 (minus2 x (lit 27)) (minus2 x (lit 27))
>     comp = hlam $ \x -> app l2 (app poly x)
>     go :: Integer -> Double -> IO ()
>     go i w | i < 200 = do
>       when (isSquare i) $ print w
>       go (1 + i) $ w - 0.001 * snd (unEval (unWDiff $ noEnv comp) () (w, 1))
>     go i w = return ()
>     isSquare n = sq * sq == n
>       where sq = floor $ sqrt (fromIntegral n::Double)
