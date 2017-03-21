> {-# LANGUAGE
>     MultiParamTypeClasses,
>     RankNTypes,
>     ScopedTypeVariables,
>     FlexibleInstances,
>     FlexibleContexts,
>     UndecidableInstances,
>     PolyKinds,
>     LambdaCase,
>     NoMonomorphismRestriction,
>     TypeFamilies,
>     LiberalTypeSynonyms,
>     EmptyCase,
>     FunctionalDependencies,
>     ExistentialQuantification,
>     InstanceSigs,
>     TupleSections #-}

This is the classical example of using sigmoid NN to approximate Xor.

> module Xor where
> import DBI hiding (return)
> import qualified Poly as YouShouldAlreadyReadThis
> import qualified Prelude as P
> import Prelude (Double, IO, return, ($), print, undefined)
> import Util

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

> weight :: DBI repr => ImpW repr h Double
> weight = ImpW id

Note that we are just manipulating AST.
If you wanna do weight sharing, you need to use let(in DDF) yourself.

Obviously, we just need to take the implicit arguments.

We have the weight, now we need the activation function, sigmoid.

> sigmoid = hlam $ \x -> recip1 (plus2 litOne (exp1 (invert1 x)))
> sigmoid1 = app sigmoid

With weight and sigmoid we can construct a neuron of type ((Double, Double) -> Double)
The weight should be a pair of Double, each as a scale on the actual input.
We then add the two scaled input and pass them into sigmoid.

> neuron = ImpW $ hlam2 $ \w p -> sigmoid1 (plus2 (mult2 (zro1 w) (zro1 p)) (mult2 (fst1 w) (fst1 p)))
> neuron1 = app neuron

Now, the hidden layer of type ((Double, Double) -> (Double, Double))

> hidden = hlam $ \p -> mkProd2 (neuron1 p) (neuron1 p)

And finally, the whole NN:

> xor :: DBI repr => ImpW repr h ((Double, Double) -> Double)
> xor = com2 neuron hidden

> main :: IO ()

Let's try to print the thing.

> main = case noEnv xor of ImpW xor -> inner xor
>   where
>     inner :: (forall w. (Term DBI () (w -> ((Double, Double) -> Double)))) -> IO ()
>     inner = undefined

now you will see a list of gibberish
