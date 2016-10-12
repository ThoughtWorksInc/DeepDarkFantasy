# DeepDarkFantasy

[![Join the chat at https://gitter.im/ThoughtWorksInc/DeepDarkFantasy](https://badges.gitter.im/ThoughtWorksInc/DeepDarkFantasy.svg)](https://gitter.im/ThoughtWorksInc/DeepDarkFantasy?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/ThoughtWorksInc/DeepDarkFantasy.svg?branch=master)](https://travis-ci.org/ThoughtWorksInc/DeepDarkFantasy)

### What if we combine Functional Programming and Deep Learning?

As we all know, a neural network is just a computable math expression (and hence a program). 

**Can we add 'ordinary' programming construct to a 'neural network', like branch, loop, pair, sum, list, and function?** 

Of course, I must still be able to train the network.

**Yes! I had add all the above construct, and I am planning to add more (stream, goto, exception, assignment, for example).** 

They all had their own special gradient structure to propagate loss accordingly. 

However, in the end of the day, what is updated is only container of double (or other representation of real). Having those construct only make you right networks easier, but does not offer fundamentally different learning capability.

----------

Can we make the language typed so we can detect error before we train the network?

**Yes and No.** I am able to type most stuff, but I am having trouble adding higher kinded type, so it is somewhat limited. I need a bit help here.

----------

Can we make the language modular and extensible so all people can write all sorts of Chuck Norris move into the language?

**Yes Yes Yes!** The whole language is structured in a way very similar to [finally tagless](http://okmij.org/ftp/tagless-final/JFP.pdf), and [object algebra](https://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf). 

So, it is possible to add new operation/constructor, and still retain type safety.

In fact, there isn't even a core language! All feature(function, double, back propagation, pretty printing) is added as ordinary plugin so you can extend/subset the language as you can wish.

# Patchouli Go!

Document is coming up. For now you can only read the code.

Example is [here](https://github.com/ThoughtWorksInc/DeepDarkFantasy/blob/master/src/main/scala/com/thoughtworks/DDF/Lang/Preclude.scala)

Lang is the default language that is usable right off the shelf. 

EvalLang provide the capability to Evaluate a term and Back Propagate, ShowLang can pretty print stuff, and NextLang provide syntax sugar for Lambda Expression in the Network.

However, none of those files does anymore than assembling 'feature provider' from ShowList (which pretty print operation related to List), EvalComb (which do evaluation and backpropagation on SKI/BCKW/Y Combinator), or whatNot. 

Reading EvalArrow -> EvalDouble -> EvalComb will get you know how stuff works. After that you can probably understand whatever just by tracing import dependency.

# FA Q

How is the speed?

Horrible. This is more of a proof of concept that we can use function in neural network, than something that can get you good kaggle score right off the shelf.

# Thank You Sir

This is heavily inspired by [Neural Networks, Types, and Functional Programming](http://colah.github.io/posts/2015-09-NN-Types-FP/), and my colleague, [Atry](https://github.com/Atry)
