#DEPRECATED.

# DeepDarkFantasy

[![Hackage](https://img.shields.io/hackage/v/DeepDarkFantasy.svg)](http://hackage.haskell.org/package/DeepDarkFantasy)
[![Join the chat at https://gitter.im/ThoughtWorksInc/DeepDarkFantasy](https://badges.gitter.im/ThoughtWorksInc/DeepDarkFantasy.svg)](https://gitter.im/ThoughtWorksInc/DeepDarkFantasy?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/DeepDarkFantasy.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/DeepDarkFantasy)
[![Coverage Status](https://coveralls.io/repos/github/ThoughtWorksInc/DeepDarkFantasy/badge.svg?branch=master)](https://coveralls.io/github/ThoughtWorksInc/DeepDarkFantasy?branch=master)

### What if we combine Functional Programming and Deep Learning?

As we all know, a neural network is just a computable math expression (and hence a program). 

**Can we add 'ordinary' programming construct to a 'neural network', like branch, loop, pair, sum, list, and function?** 

Of course, I must still be able to train the network.

**Yes! I had add all the above construct, and I am planning to add more.** 

They all had their own special gradient structure to propagate loss accordingly. 

However, in the end of the day, what is updated is only container of double (or other representation of real).
Having those construct only make you write networks easier, but does not offer fundamentally different learning capability.

----------

Can we make the language typed so we can detect error before we train the network?

**Sort of.** I am able to type most stuff, but I am having trouble adding higher kinded type/generic type. However, they can be written as Haskell function (macro in DDF).

----------

Can we make the language modular and extensible so all people can write all sorts of Chuck Norris move into the language?

**Yes Yes Yes!** The whole language is structured in [finally tagless style](http://okmij.org/ftp/tagless-final/JFP.pdf), so, it is possible to add new operation/constructor, and still retain type safety.

In fact, there isn't even a core language! All feature(function, double, back propagation, pretty printing) is added as ordinary plugin so you can extend/subset the language as you can wish.

# Patchouli Go!

You should read the [blog](http://marisa.moe/2017/DLPL/) before anything.

We have a few example on using DDF:

[Hello world](DDF/Sam/Hello.lhs)

[Solving polynomial equation](DDF/Sam/Poly.lhs)

[Training XOR Network](DDF/Sam/Xor.lhs)

If you want to look into the code base, it is necessary to understand [Finally Tagless](http://www.cs.cornell.edu/info/projects/nuprl/PRLSeminar/PRLSeminar2011/Chung-chiehShan-FinallyTaglessPartiallyEevaluated.pdf).

# FA Q

Q: How is the speed?

A: Unoptimized. This is more of a proof of concept that we can use function in neural network, than something that can get you good kaggle score right off the shelf. We are working on Partial Evaluation.

Q: Why does this work theoretically?

A: See [DDFADC](https://github.com/MarisaKirisame/DDFADC)

Q: What does this have to do with [Yang Bo](https://github.com/Atry)'s [DeepLearning.scala](https://github.com/ThoughtWorksInc/DeepLearning.scala/)?

A: We work on a prototype for 2-3 months, and split apart.

Q: You seems to have a space in FAQ.

A: I like it that way.

Q: What are you currently working on?

A: I am trying to add a neural network demo.

# Thank You Sir

This is heavily inspired by [Neural Networks, Types, and Functional Programming](http://colah.github.io/posts/2015-09-NN-Types-FP/), and my colleague, [Yang Bo](https://github.com/Atry).

Also, I'd like to thanks [dram](https://github.com/dramforever) for getting it to work without Incoherent Instances, and fixing it on stack, cabal & travis.

And [izgzhen](https://github.com/izgzhen) helps with the initial version of Partial Evaluation.

You can be the next contributor!

![I Want You](img/I_Want_You.png "Baka!")
[image courtesy of Milk Mage](http://www.pixiv.net/member_illust.php?id=461351)
[aquired here](img/I_Want_You_Image_Courtesy.png)

Please look at [This Issue](https://github.com/ThoughtWorksInc/DeepDarkFantasy/issues/174) and help solve it.
