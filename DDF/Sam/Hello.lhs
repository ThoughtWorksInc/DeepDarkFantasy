> {-# LANGUAGE
>   NoImplicitPrelude,
>   ExplicitForAll,
>   KindSignatures,
>   NoMonomorphismRestriction
> #-}

> module DDF.Sam.Hello where
> import DDF.Lang
> import DDF.Eval
> import DDF.Show
> import qualified Prelude as M

DDF is a language embedded in Haskell, written using Finally Tagless Style.
Being an embedded language mean it is a piece of cake to write DDF macro - they are just Haskell function!
Another benefit is that DDF code is typed check when GHC compile, so you can do type driven developement.

> hello :: forall (int :: * -> * -> *) (h :: *). Lang int => int h String
> hello = string "Hello"

Let's get to action!
The code above define a literal in hello.
Note that it has strange type: it take an (int :: * -> * -> *), and (h :: *), and return int h String.
Well, h is an enviroment of variable, and int h is an interpreter which interpret term in enviroment h.
int h String mean the final result is an int, so we had constrained the term to be scope-correct and type safe at Haskell compile time.
Lang int is simply the constraint that int can interpret anything inside Lang.

> world :: (List int, Char int) => int () String
> world = string "world"

Well, we are just defining string literal, so it don't need the full power of Lang.
Since string is list of char, using List and Char should be enough.
Also note that we had manually select an enviromnet, instead of letting it be anything.
The enviroment is simply (), which mean there's no free variable.

> space :: (List int, Char int) => int h (String -> String -> String)
> space = lam2 $ \l r -> listAppend2 l (cons2 (char ' ') r)

And now we should play with lambda a bit.
Lambda abstraction is just Haskell Lambda abstraction (with some magic), so it is relatively easy to use.
Also, note how everything is scope safe again: it is impossible to append x to z, because variable z doesnt exist.
It is assumed that every interpreter know how to deal with lambda, so we dont need another constraint (it is implied by both List and Char).

> addTail :: (List int, Char int) => int h String -> M.Char -> int h String
> addTail l r = listAppend2 l (cons2 (char r) nil)

Now this is some macro: just ordinary Haskell function that generate AST.
They can take both Haskell term, DDF term as input, and notice that the macro is type safe.

> str = addTail (app2 space hello world) '!'

Finally, notice that, under NoMonomorphismRestriction, GHC will infer the most general type automatically.
Unfortunately, since we specify Lang, and use () as env in the component, it's type are more restrictive than we hope.

> main :: M.IO ()
> main = do
>   print $ runEval str ()
>   print $ showAST str
>   M.return ()

Now we show two interpreter: the evaluator and the pretty printer.
One output the final result and one output the AST.
The () floating around is the enviroment we feed into the evaluator.
