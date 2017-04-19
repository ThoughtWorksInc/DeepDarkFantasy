> {-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, TypeApplications, RankNTypes #-}

This is the classical example of using sigmoid NN to approximate Xor.
You should already read DDF.Poly before this.

> module DDF.Xor where
> import qualified Prelude as M
> import System.Random
> import Control.Monad (when)
> import Data.Proxy
> import Data.Constraint
> import DDF.Util
> import DDF.DLang
> import DDF.Show
> import DDF.Combine ()
> import DDF.Eval ()
> import DDF.GWDiff ()
> import DDF.ImpW
> import DDF.WithDiff
> import qualified DDF.Meta.Dual as M
> import DDF.InfDiff ()

Recall in poly, we constructed a function Double -> Double,
with argument being the weight, and do gradient descend to found a solution.

However, most of the time, there wil be more than one weight (or no weight at all).
Also, when we are composing a Neural Network to approximate a value, we dont really care how much weight it use.
So, we use existential type to hide the actual weight.

data ImpW repr h x = forall w. Weight w => ImpW (repr h (w -> x))

ImpW stands for implicit weights.
The existential w is weight, and a Neural Network of type x is just a function from w to x!
We require that the weight can be constructed randomly, so we have random initialization.
Weight also form a Vector so we can combine weights (update it), scale it (to control the learning rate).

Let's start by constructing a weight.

> doubleWeight :: DLang repr => ImpW repr h M.Double
> doubleWeight = ImpW id

Note that we are just manipulating AST.
If you wanna do weight sharing, you need to use let(in DDF) yourself.

Obviously, we just need to take the implicit argument.

We have the weight, now we need the activation function, sigmoid.

> sigmoid = lam $ \x -> recip1 (plus2 doubleOne (doubleExp1 (invert1 x)))
> sigmoid1 = app sigmoid

With weight and sigmoid we can construct a neuron of type ((M.Double, M.Double) -> M.Double)
The weight should be a pair of M.Double, each as a scale on the actual input, with a bias.
We then add the two scaled input, with the bias, and pass them into sigmoid.

> scaleAdd :: DLang repr => ImpW repr h ((M.Double, M.Double) -> M.Double)
> scaleAdd = ImpW $ lam2 $ \w p -> plus2 (mult2 (zro1 w) (zro1 p)) (plus2 (fst1 w) (fst1 p))

> withBias :: DLang repr => ImpW repr h (M.Double -> M.Double)
> withBias = ImpW $ plus

> neuron :: DLang repr => ImpW repr h ((M.Double, M.Double) -> M.Double)
> neuron = com2 (com2 sigmoid withBias) scaleAdd
> neuron1 = app neuron

Now, the hidden layer of type (M.Double, M.Double) -> ((M.Double, M.Double), (M.Double, M.Double))

> hidden = lam $ \p -> mkProd2 (mkProd2 (neuron1 p) (neuron1 p)) (mkProd2 (neuron1 p) (neuron1 p))

And finally, the whole NN:

> type XOR = (M.Double, M.Double) -> M.Double
> xorNet :: DLang repr => ImpW repr h XOR
> xorNet = neuron `com2` (bimap2 scaleAdd scaleAdd) `com2` hidden

But before we can train it, we need to define the dataset and the loss function.

> l2 :: DLang repr => repr h (M.Double -> M.Double -> M.Double)
> l2 = lam2 $ \l r -> (mult2 (minus2 l r) (minus2 l r))
> l22 = app2 l2

> eval :: DLang repr => repr h (XOR -> ((M.Double, M.Double), M.Double) -> M.Double)
> eval = lam2 $ \xor p -> l22 (app xor (zro1 p)) (fst1 p)

> dataset :: DLang repr => repr h [((M.Double, M.Double), M.Double)]
> dataset = cons2 (build 0 0 0) (cons2 (build 0 1 1) (cons2 (build 1 0 1) (cons2 (build 1 1 0) nil)))
>   where build l r ret = mkProd2 (mkProd2 (double l) (double r)) (double ret)

However, unlike Poly, there are more than one datapoint, so we need to use a list, and map xor onto it.

> loss :: DLang repr => repr h (XOR -> M.Double)
> loss = lam $ \xor -> fix2 (lam $ \self -> listMatch2 doubleZero (lam2 $ \x xs -> plus2 x (app self xs))) (map2 (app eval xor) dataset)

Now we are good to implement the train function!

> findXor :: forall g m. (RandomGen g, M.Monad m) => g -> (AST -> m ()) -> (Int -> M.Double -> M.String -> m ()) -> m XOR
> findXor rand doAST doIter = case runImpW $ noEnv xorNet of
>   RunImpW ((Combine (Show xorS) (Combine (Eval xorEv) xorE)) :: Weight w => Combine Show (Combine Eval (GWDiff Eval)) () (w -> XOR)) -> do
>     doAST $ xorS vars 0

printing weights. now you will see a list of gibberish

>     let initWeight :: w = M.fst $ ((randomR (randRange (-0.01, 0.01)) \\ weightCon @w @Random) \\ weightCon @w @RandRange) rand

Getting random weights...

>     (go (diff xorE) initWeight (runEval selfWithDiff () \\ weightCon @w @(WithDiff Eval)) (diff loss)
>         ((runEval (lam3 $ \d o n -> minus2 o (mult2 d n)) ()) \\ weightCon @w @(Vector Eval)) 0 (xorEv ())) \\ weightCon @w @M.Show
>     where
>       diff :: GWDiff Eval () x -> Diff w x
>       diff x = ((runEval (runGWDiff x (Proxy :: Proxy w)) ()) \\ weightCon @w @(Vector Eval)) \\ weightCon @w @(Vector (InfDiff Eval))
>       go :: M.Show w => (Diff w (w -> XOR)) -> w -> (w -> Diff w w) -> (Diff w (XOR -> M.Double)) -> (M.Double -> w -> w -> w) -> Int -> (w -> XOR) -> m XOR
>       go xor weight reifyE lossE update i orig | i <= 2500 = do
>         doIter i lossVal (M.show weight)
>         go xor (update 0.3 weight lossDiff) reifyE lossE update (1 + i) orig
>           where
>             M.Dual (lossVal, lossDiff) = lossE $ xor (reifyE weight)
>       go _ weight _ _ _ _ orig = M.return $ orig weight

> main :: IO ()
> main = do
>   g <- getStdGen
>   xorTrained <- findXor g print (\i d w -> when (isSquare i) $ do
>     print d
>     M.putStrLn w
>     M.putStrLn "")
>   let doXor :: M.Double -> M.Double -> IO ()
>       doXor l r = M.putStrLn $ M.show l ++ " xor " ++ M.show r ++ " is " ++ (M.show $ xorTrained (l, r))
>   doXor 0 0
>   doXor 0 1
>   doXor 1 0
>   doXor 1 1
>   M.return ()
