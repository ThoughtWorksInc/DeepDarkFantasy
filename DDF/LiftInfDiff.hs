{-# LANGUAGE TypeFamilies, NoImplicitPrelude, FlexibleInstances, RankNTypes, LambdaCase, InstanceSigs, ScopedTypeVariables, FlexibleContexts #-}

module DDF.LiftInfDiff where

import DDF.DLang
import DDF.Eval ()
import DDF.InfDiff ()
import DDF.UnLiftEnv
import qualified Prelude as M

newtype LiftInfDiff r h x = LiftInfDiff {runLiftInfDiff :: UnLiftEnv (LiftInfDiffNoEnv r) h x}
newtype LiftInfDiffNoEnv r h x = LiftInfDiffNoEnv {runLiftInfDiffNoEnv :: r () (InfDiff LiftInfDiffLang () x)}
newtype LiftInfDiffLang h x = LiftInfDiffLang {runLiftInfDiffLang :: LiftInfDiffType x}

type family LiftInfDiffType h

type instance LiftInfDiffType M.Double = M.Double
type instance LiftInfDiffType () = ()
type instance LiftInfDiffType (x -> y) = LiftInfDiffNoEnv Eval () x -> LiftInfDiffNoEnv Eval () y
type instance LiftInfDiffType (l, r) = (LiftInfDiffNoEnv Eval () l, LiftInfDiffNoEnv Eval () r)

lec x = LiftInfDiffNoEnv $ Eval $ \_ -> x
lulec x = LiftInfDiff $ UnLiftEnv $ lec x
lulea (LiftInfDiff (UnLiftEnv (LiftInfDiffNoEnv (Eval x)))) = x ()
lea (LiftInfDiffNoEnv (Eval x)) = x ()
icl (InfDiff (Combine (LiftInfDiffLang x) _)) = x
icg (InfDiff (Combine _ (GDiff x))) = x
icllea = icl . lea

instance DBI (LiftInfDiff Eval) where
  z = lulec zImpl
    where
      zImpl :: forall a h. InfDiff LiftInfDiffLang () ((a, h) -> a)
      zImpl = InfDiff $ Combine (LiftInfDiffLang $ (\case (a, _) -> a) . icllea) (GDiff $ \_ -> Diff zImpl)
  s x' = lulec $ sImpl $ lulea x'
    where
      sImpl :: forall a h b. InfDiff LiftInfDiffLang () (h -> b) -> InfDiff LiftInfDiffLang () ((a, h) -> b)
      sImpl x =
        InfDiff $ Combine (LiftInfDiffLang $ (\case (_, h) -> icl x h) . icllea) (GDiff $ \p -> Diff $ sImpl $ runDiff $ icg x p)
  app f' x' = lulec $ appImpl (lulea f') (lulea x')
    where
      appImpl :: forall h a b. InfDiff LiftInfDiffLang () (h -> a -> b) -> InfDiff LiftInfDiffLang () (h -> a) -> InfDiff LiftInfDiffLang () (h -> b)
      appImpl f x = InfDiff $ Combine (LiftInfDiffLang $ \h -> icllea (icl f h) (icl x h)) (GDiff $ \p -> Diff $ appImpl (runDiff $ icg f p) (runDiff $ icg x p))
  abs x' = lulec $ absImpl (lulea x')
    where
      absImpl :: forall a h b. InfDiff LiftInfDiffLang () ((a, h) -> b) -> InfDiff LiftInfDiffLang () (h -> a -> b)
      absImpl x = InfDiff $ Combine (LiftInfDiffLang $ \h -> lec $ absImplAux x (lea h)) (GDiff $ \p -> Diff $ absImpl (runDiff $ icg x p))
      absImplAux :: forall a h b. InfDiff LiftInfDiffLang () ((a, h) -> b) -> InfDiff LiftInfDiffLang () h -> InfDiff LiftInfDiffLang () (a -> b)
      absImplAux x h =
        InfDiff $ Combine
          (LiftInfDiffLang $ \a -> icl x (lec $ absImplAuxAux (lea a) h))
          (GDiff $ \p -> Diff $ absImplAux (runDiff $ icg x p) (runDiff $ icg h p))
      absImplAuxAux :: forall a h. InfDiff LiftInfDiffLang () a -> InfDiff LiftInfDiffLang () h -> InfDiff LiftInfDiffLang () (a, h)
      absImplAuxAux a h = InfDiff $ Combine (LiftInfDiffLang (lec a, lec h)) (GDiff $ \p -> Diff $ absImplAuxAux (runDiff $ icg a p) (runDiff $ icg h p))
  liftEnv x' = lulec $ liftEnvImpl $ lulea x'
    where
      liftEnvImpl :: forall h x. InfDiff LiftInfDiffLang () (() -> x) -> InfDiff LiftInfDiffLang () (h -> x)
      liftEnvImpl x = InfDiff $ Combine (LiftInfDiffLang $ \_ -> lec $ liftEnvImplAux x) (GDiff $ \p -> Diff $ liftEnvImpl (runDiff $ icg x p))
      liftEnvImplAux :: forall x. InfDiff LiftInfDiffLang () (() -> x) -> InfDiff LiftInfDiffLang () x
      liftEnvImplAux x = InfDiff $ Combine (LiftInfDiffLang $ icllea $ (icl x $ lec liftEnvImplAuxAux)) (GDiff $ \p -> Diff $ liftEnvImplAux (runDiff $ icg x p))
      liftEnvImplAuxAux :: InfDiff LiftInfDiffLang () ()
      liftEnvImplAuxAux = InfDiff $ Combine (LiftInfDiffLang ()) (GDiff $ \_ -> Diff $ liftEnvImplAuxAux)

diff :: forall v h x. Proxy v -> LiftInfDiff Eval h (x -> DiffType v x)
diff v = lulec diffImpl
  where
    diffImpl :: forall h x. InfDiff LiftInfDiffLang () (h -> x -> DiffType v x)
    diffImpl = InfDiff $ Combine (LiftInfDiffLang $ \_ -> lec diffImplAux) (GDiff $ \p -> Diff $ 2)
    diffImplAux :: forall x. InfDiff LiftInfDiffLang () (x -> DiffType v x)
    diffImplAux = InfDiff $ Combine (LiftInfDiffLang $ \x -> lec $ runDiff $ icg (lea x) v) 4