{-# LANGUAGE RankNTypes, TypeFamilies, LambdaCase, FlexibleContexts #-}

module DDF.RTDiff where

import DDF.Lang

data RTDiff r h x = RTDiff (r (InfDiffEnv h) (InfDiffTerm x)) (forall v. Vector r v => Proxy v -> RTDiff r h (Diff v x))

--instance DBI r => DBI (RTDiff r) where
-- app (RTDiff lc ln) (RTDiff rc rn) = RTDiff (app lc rc) (\p -> app (ln p) (rn p))

rtDiff :: (Vector r v, DBI (RTDiff r)) => Proxy v -> RTDiff r h (x -> Diff v x)
rtDiff p = hoas (\case (RTDiff _ n) -> n p)
--instance Lang r => Lang (RTDiff r)