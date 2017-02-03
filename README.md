# DeepDarkFantasy

[![Join the chat at https://gitter.im/ThoughtWorksInc/DeepDarkFantasy](https://badges.gitter.im/ThoughtWorksInc/DeepDarkFantasy.svg)](https://gitter.im/ThoughtWorksInc/DeepDarkFantasy?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/DeepDarkFantasy.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/DeepDarkFantasy)

### What if we combine Functional Programming and Deep Learning?

As we all know, a neural network is just a computable math expression (and hence a program). 

**Can we add 'ordinary' programming construct to a 'neural network', like branch, loop, pair, sum, list, and function?** 

Of course, I must still be able to train the network.

**Yes! I had add all the above construct, and I am planning to add more (stream, tree, map, set, for example).** 

They all had their own special gradient structure to propagate loss accordingly. 

However, in the end of the day, what is updated is only container of double (or other representation of real).
Having those construct only make you right networks easier, but does not offer fundamentally different learning capability.

The full list of features implemented can be seen [here](FEATURES.md)

----------

Can we make the language typed so we can detect error before we train the network?

**Yes and No.** I am able to type most stuff, but I am having trouble adding higher kinded type, so it is somewhat limited. I need a bit help here.

----------

Can we make the language modular and extensible so all people can write all sorts of Chuck Norris move into the language?

**Yes Yes Yes!** The whole language is structured in a way very similar to [finally tagless](http://okmij.org/ftp/tagless-final/JFP.pdf), and [object algebra](https://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf). 

So, it is possible to add new operation/constructor, and still retain type safety.

In fact, there isn't even a core language! All feature(function, double, back propagation, pretty printing) is added as ordinary plugin so you can extend/subset the language as you can wish.

# Patchouli Go!

You should read the [blog](http://marisa.moe/DLPL.html) before anything.

We have an [annotated example](doc/poly.md) on how to use our code. More documentation is coming up.

Another good starting point is [here](src/main/scala/com/thoughtworks/DDF/Language/Preclude.scala).

If you want to look into the code base, it is necessary to understand [Finally Tagless](http://www.cs.cornell.edu/info/projects/nuprl/PRLSeminar/PRLSeminar2011/Chung-chiehShan-FinallyTaglessPartiallyEevaluated.pdf)
# FA Q

Q: How is the speed?

A: Unoptimized. This is more of a proof of concept that we can use function in neural network, than something that can get you good kaggle score right off the shelf.

Q: Why does this work theoretically?

A: See [DDFADC](https://github.com/MarisaKirisame/DDFADC)

Q: What does this have to do with [Atry](https://github.com/Atry)'s [DeepLearning.scala](https://github.com/ThoughtWorksInc/DeepLearning.scala/)?

A: We work on a prototype for 2-3 months, and split apart.

Q: You seems to have a space in FAQ.

A: I like it that way.

# Thank You Sir

This is heavily inspired by [Neural Networks, Types, and Functional Programming](http://colah.github.io/posts/2015-09-NN-Types-FP/), and my colleague, [Atry](https://github.com/Atry)
