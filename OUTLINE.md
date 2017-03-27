Neural Network are program, and we can use PL techinque to help build Neural Network.

What is NN?
NN approach blackbox function.
Very general - NLP, CV, speech synthesis, a few work on program synthesis/automatic theorem proving!
How?

Step back: solve x ^ 2 + 2 x + 3 = 27, without using analyatic solution.
What does it mean to 'solve it'?
We want to find the best x.
What is the best x?
lhs closer to 27, better it is.

Transform into a optimization problem:
minimize opt = (x ^ 2 + 2 x - 24) ^ 2
but how? Hill climbing (actually gradient descending)
find initial x, if d opt/d x is positive, decrease x, else increase x.
newX = x - learningRate * d opt/d x

Neural Network: a generic computation structure with a lot of unknown variable (weights).
The structure is usually specified by human.
Training a Neural Network: finding the best weights with mathematical optimization, like gradient descend, variant of newton method.

How to find derivative?
dual number approach
We transform every Double into (Double, Double).
Generic function get transformed into themself, with the right type parameter

what does derivative of a program mean? (TODO: expand)
Proof:

example on Poly
\x -> (x * x + (2 x + 3))
we start by translating it into the deriv form:
give a (Double, Double), where the left component is a value for x
and the right component is the derivative of x with respect to an unknwon variable
what unknown variable? We dont know, we dont care. Any will do, even an unrelated variable (in that case right component is 0)
will return a (Double, Double), with left being the original result, right being the derivative of left part wrt that unknown variable.
to find derivative of x:
plug in (x, 1) for x
walk with you step by step:
(x, 1) * (x, 1) + ((2, 0) * (x, 1) + (3, 0))
constant have 0 as derivative.
(x, 1) * (x, 1) + ((2 * x, 0 * x + 1 * 2) + (3, 0))
(x, 1) * (x, 1) + ((2 * x, 2) + (3, 0))
(x, 1) * (x, 1) + (2 * x + 3, 2 + 0)
(x, 1) * (x, 1) + (2 * x + 3, 2)
(x * x, 1 * x + 1 * x) + (2 * x + 3, 2)
(x * x, 2 * x) + (2 * x + 3, 2)
(x * x + 2 * x + 3, 2 * x + 2)
extract the right part, use it to update the diff, repeat
run program here

Problem: multiple variable
generalized dual number
the right part need only be a vector
plus, zero, invert, scale
product type is vector (if left and right is vector)

what does it mean now? (TODO: expand)

back propagation
There might be million of variable to diff on.
Typical in Neural Network.
Vector dom, Vector rng => Vector (dom -> rng)
scaling: do not 'look inside rng'
just scale dom and pass it instead
+: distribute the dom, add two result

NN = existential weight
Some more requirement (show, random init, ...). TODO: expand
example xor network

Lie: problem still exist
Cannot guarentee that a backprop function will be called once.
Might be called multiple times, with that definition of +
Which call the inner multiple times too... Exponential blowup, yay.
Back propagation done right
wengret list? 'Free vector'
use an AST with plus/zero/invert/scale.
building AST is really fast (comparing to the exponential solution above)

Partial Evaluation
AST is fast comparing to exponential blowup, but is really really slow:
Cannot work on GPU, also dynamic eval overhead!
Partial Evaluation, get the AST away (provided that the control flow is fixed for fixed input, varying weight)

PL/FP technique:
Optimization
Partial Evaluation
Type Checking
Typeclass: unify forward mode, backward mode, multiple variable, no variable!
Final Tagless style - no need to handle NN specially
Closure of diff operator: higher order derivative for free! (We just take derivative multiple times)
example from Colah's blog

FAQ?
