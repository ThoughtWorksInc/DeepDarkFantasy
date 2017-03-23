What is NN?
NN approach blackbox function.
How?

Step back:
solve x ^ 2 + 2 x + 3 = 27, without using analyatic solution.

What does it mean to 'solve it'? What is the best x?

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

what does derivative of a program mean

example on Poly

Problem: multiple variable

generalized dual number

what does it mean now?

back propagation

NN = existential weight

example xor network

Lie: problem still exist

Back propagation done right

Partial Evaluation