#####This example use gradient descent to find a best value.
Let's first import some stuff:
```tut
import com.thoughtworks.DDF.Gradient.GDouble
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.{NoInfo, Show}
```
Now we make a shorthand to declare AST:
```tut
val l = InterLangTermLang
```
We then introduce a variable. Let's called it x.
```tut
val nl = NextLang(l, l.doubleInfo)
```
We build the AST representing "x * x + 2 * x + 3".

Beside all the straightforward functions, nl.in is our variable, namely x.

We use nl.collapse to compile AST with variable into standard AST.

Now the AST does not contain a free variable "x", and become a AST that represent a function accepting a double (our "x"), 
returning a double
```tut
val exp = nl.collapse(nl.plusD__(nl.multD__(nl.in)(nl.in))(nl.plusD__(nl.multD__(nl.litD(2))(nl.in))(nl.litD(3))))
```
We then build "(x - 27) * (x - 27)".

Let is of type A => (A => B) => B, with A, B specialize to double

W (W = f => x => f(x)(x)) is of type (A => A => B) => (A => B). 

W(mult) take a double x, and return x * x. It is just square. 
```tut
val loss = nl.collapse(nl.Let__(nl.minusD__(nl.litD(27))(nl.in))(nl.W_(nl.multD)))
```
Now we use B: (B => C) => (A => B) => (A => C) to compose loss after exp, 
so for a x, we can measure how close it is to 27.
```tut
val train = l.B__(loss)(exp)
```
We do find the AST representing the derivative of the whole pass:

ADEvalInterLang is a intepreter of the AST: it just return a new AST representing the derivative.
```tut
val train_it: LangTerm[((Double, Double)) => (Double, Double)] =
  train(ADEvalInterLang).get[Double](
    ADEvalInterLang.aInfo(ADEvalInterLang.doubleInfo, ADEvalInterLang.doubleInfo))(GDouble)
```
We set inital weight(x) to 0. 
This shouldnt be done to neural network in general, but it has non zero derivative so we will do so.
```tut
var weight: Double = 0
```
We can print the derivative AST. It is very long and unreadable, but dont worry about that. 
We will run it to give you confidence.

ShowLang is the interpreter that print stuff.
```tut
train_it[NoInfo, Lambda[X => Show]](ShowLang)
```
Here's the standard gradient descend.

EvalMInterLang is the intepreter that evaluate it in MetaLanguage(scala).

We can also evaluate it in ObjectLanguage(DDF) but we dont need that.
```tut
for (_ <- Range(0, 100)) {
  for (_ <- Range(0, 2)) {
    weight -= 0.01 * train_it(InterLangTermLang)[NoInfo, Lambda[X => X]](EvalMInterLang)((weight, 1))._2
  }
  println(weight)
}
```
As you can see, weight approach 4, because 4 * 4 + 2 * 4 + 3 = 27.