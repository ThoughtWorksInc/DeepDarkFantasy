> {-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, TypeApplications #-}

This is the classical example of using sigmoid NN to approximate Xor.

> module Xor where
> import DBI hiding (return)
> import qualified Poly as YouShouldAlreadyReadThis
> import qualified Prelude as P
> import Prelude (Double, IO, return, ($), print, undefined, Int, (<=), (<), (+), (-), (*), (/), fromIntegral, putStr)
> import Util
> import System.Random
> import Control.Monad (when)
> import Data.Proxy
> import Data.Constraint
> import Lang

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

> weight :: Lang repr => ImpW repr h Double
> weight = ImpW id

Note that we are just manipulating AST.
If you wanna do weight sharing, you need to use let(in DDF) yourself.

Obviously, we just need to take the implicit argument.

We have the weight, now we need the activation function, sigmoid.

> sigmoid = lam $ \x -> recip1 (plus2 doubleOne (doubleExp1 (invert1 x)))
> sigmoid1 = app sigmoid

With weight and sigmoid we can construct a neuron of type ((Double, Double) -> Double)
The weight should be a pair of Double, each as a scale on the actual input, with a bias.
We then add the two scaled input, with the bias, and pass them into sigmoid.

> scaleAdd :: Lang repr => ImpW repr h ((Double, Double) -> Double)
> scaleAdd = ImpW $ lam2 $ \w p -> plus2 (mult2 (zro1 w) (zro1 p)) (plus2 (fst1 w) (fst1 p))

> withBias :: Lang repr => ImpW repr h (Double -> Double)
> withBias = ImpW $ plus

> neuron :: Lang repr => ImpW repr h ((Double, Double) -> Double)
> neuron = com2 (com2 sigmoid withBias) scaleAdd
> neuron1 = app neuron

Now, the hidden layer of type (Double, Double) -> ((Double, Double), (Double, Double))

> hidden = lam $ \p -> mkProd2 (mkProd2 (neuron1 p) (neuron1 p)) (mkProd2 (neuron1 p) (neuron1 p))

And finally, the whole NN:

> type XOR = (Double, Double) -> Double
> xor :: Lang repr => ImpW repr h XOR
> xor = neuron `com2` (bimap2 scaleAdd scaleAdd) `com2` hidden

But before we can train it, we need to define the dataset and the loss function.

> l2 :: Lang repr => repr h (Double -> Double -> Double)
> l2 = lam2 $ \l r -> (mult2 (minus2 l r) (minus2 l r))
> l22 = app2 l2

> eval :: Lang repr => repr h (XOR -> ((Double, Double), Double) -> Double)
> eval = lam2 $ \xor p -> l22 (app xor (zro1 p)) (fst1 p)

> dataset :: Lang repr => repr h [((Double, Double), Double)]
> dataset = cons2 (build 0 0 0) (cons2 (build 0 1 1) (cons2 (build 1 0 1) (cons2 (build 1 1 0) nil)))
>   where build l r ret = mkProd2 (mkProd2 (double l) (double r)) (double ret)

However, unlike Poly, there are more than one datapoint, so we need to use a list, and map xor onto it.

> loss :: Lang repr => repr h (XOR -> Double)
> loss = lam $ \xor -> fix2 (lam $ \self -> listMatch2 doubleZero (lam2 $ \x xs -> plus2 x (app self xs))) (map2 (app eval xor) dataset)

Let's try to print the thing.

> main :: IO ()
> main = case runImpW $ noEnv xor of RunImpW xor -> inner xor
>   where
>     inner :: forall w. Weight w => Combine Show (GWDiff Eval) () (w -> (Double, Double) -> Double) -> IO ()
>     inner (Combine xorS xorE) = do
>       print $ runShow xorS vars 0

now you will see a list of gibberish

>       initWeight :: w <- ((randomRIO (randRange (-0.01, 0.01)) \\ weightCon @w @Random) \\ weightCon @w @RandRange)

Getting random weights...

>       (go (diff xorE) initWeight (runEval selfWithDiff () \\ weightCon @w @(WithDiff Eval)) (diff loss)
>           ((runEval (lam3 $ \d o n -> minus2 o (mult2 d n)) ()) \\ weightCon @w @(Vector Eval)) 0) \\ weightCon @w @P.Show
>         where
>           diff :: GWDiff Eval () x -> Diff w x
>           diff x = (runEval (runGWDiff x (Proxy :: Proxy w)) ()) \\ weightCon @w @(Vector Eval)
>     go :: P.Show w => (Diff w (w -> XOR)) -> w -> (w -> Diff w w) -> (Diff w (XOR -> Double)) -> (Double -> w -> w -> w) -> Int -> IO ()
>     go xor weight reify loss update i | i <= 2500 = do
>       when (isSquare i) $ do
>         print weight
>         print lossVal
>         putStr "\n"
>       go xor (update 0.3 weight lossDiff) reify loss update (1 + i)
>       where
>         (lossVal, lossDiff) = loss $ xor (reify weight)
>     go _ _ _ _ _ _ = return ()

And the main loop. Basically we take the derivative of stuff, eval them, and use the derivative of loss to update the weights.