#####Using gradient descent to solve x*x+2x+3=27.
Let's first import some stuff:
```scala
scala> import com.thoughtworks.DDF.Gradient.GDouble
import com.thoughtworks.DDF.Gradient.GDouble

scala> import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.Language._

scala> import com.thoughtworks.DDF.{NoInfo, Show}
import com.thoughtworks.DDF.{NoInfo, Show}
```
Now we make a shorthand to declare AST:
```scala
scala> val l = InterLangTermLang
l: com.thoughtworks.DDF.Language.InterLangTermLang.type = com.thoughtworks.DDF.Language.InterLangTermLang$@182a75a
```
We then introduce a variable. Let's called it x.
```scala
scala> val nl = NextLang(l, l.doubleInfo)
nl: com.thoughtworks.DDF.Language.Lang[com.thoughtworks.DDF.Language.InterLangInfoG,[X]scala.util.Either[com.thoughtworks.DDF.Language.InterLangTerm[X],com.thoughtworks.DDF.Language.InterLangTerm[Double => X]]] with com.thoughtworks.DDF.Language.NextBase[com.thoughtworks.DDF.Language.InterLangInfoG,com.thoughtworks.DDF.Language.InterLangTerm,Double] = com.thoughtworks.DDF.Language.NextLang$$anon$1@1f43642
```
We build the AST representing "x * x + 2 * x + 3".

Beside all the straightforward functions, nl.in is our variable, namely x.

We use nl.collapse to compile AST with variable into standard AST.

Now the AST does not contain a free variable "x", and become a AST that represent a function accepting a double (our "x"), 
returning a double
```scala
scala> val exp = nl.collapse(nl.plusD__(nl.multD__(nl.in)(nl.in))(nl.plusD__(nl.multD__(nl.litD(2))(nl.in))(nl.litD(3))))
exp: com.thoughtworks.DDF.Language.InterLangTerm[Double => Double] = com.thoughtworks.DDF.Language.InterLangTermInterLang$$anon$38@174d71c
```
We then build the loss function, "(x - 27) * (x - 27)", a L2 Loss.

Let is of type A => (A => B) => B, with A, B specialize to double

W (W = f => x => f(x)(x)) is of type (A => A => B) => (A => B). 

W(mult) take a double x, and return x * x. It is just square. 
```scala
scala> val loss = nl.collapse(nl.Let__(nl.minusD__(nl.litD(27))(nl.in))(nl.W_(nl.multD)))
loss: com.thoughtworks.DDF.Language.InterLangTerm[Double => Double] = com.thoughtworks.DDF.Language.InterLangTermInterLang$$anon$38@b55177
```
Now we use B: (B => C) => (A => B) => (A => C) to compose loss after exp, 
so for a x, we can measure how close it is to 27.
```scala
scala> val train = l.B__(loss)(exp)
train: com.thoughtworks.DDF.Language.InterLangTerm[Double => Double] = com.thoughtworks.DDF.Language.InterLangTermInterLang$$anon$38@1a25b02
```
We find the AST representing the derivative of the whole pass:

ADEvalInterLang is a interpreter of the AST: it just return a new AST representing the derivative.
```scala
scala> val train_it: LangTerm[((Double, Double)) => (Double, Double)] =
     |   train(ADEvalInterLang).get[Double](
     |     ADEvalInterLang.aInfo(ADEvalInterLang.doubleInfo, ADEvalInterLang.doubleInfo))(GDouble)
train_it: com.thoughtworks.DDF.Language.LangTerm[((Double, Double)) => (Double, Double)] = com.thoughtworks.DDF.Language.LangTermLang$$anon$39@136bf21
```
We set initial weight(x) to 0. 
This shouldn't be done to neural network in general, but it has non zero derivative so we will do so.
```scala
scala> var weight: Double = 0
weight: Double = 0.0
```
We can print the derivative AST. It is very long and unreadable, but don't worry about that. 
We will run it to give you confidence.

ShowLang is the interpreter that print stuff.
```scala
scala> train_it[NoInfo, Lambda[X => Show]](ShowLang)
res0: com.thoughtworks.DDF.Show = (B (C (B Let (B (C (B (S (B S (B (B mkPair) (C (B B (B + (B zro I))) (B zro I)))) (C (B B (B + (B fst I))) (B fst I))) (S (B S (B (B mkPair) (C (B B (B * (B zro I))) (B zro I)))) (S (B S (B (B +) (C (B B (B * (B zro I))) (B fst I)))) (B (C (B * (B zro I))) (B fst I))) (mkPair -1.0 0.0))) (mkPair 27.0 0.0)) I)) (W (S (B S (B (B mkPair) (C (B B (B * (B zro I))) (B zro I)))) (S (B S (B (B +) (C (B B (B * (B zro I))) (B fst I)))) (B (C (B * (B zro I))) (B fst I)))))) (S (B (S (B S (B (B mkPair) (C (B B (B + (B zro I))) (B zro I)))) (C (B B (B + (B fst I))) (B fst I))) (S (B (S (B S (B (B mkPair) (C (B B (B * (B zro I))) (B zro I)))) (S (B S (B (B +) (C (B B (B * (B zro I))) (B fst I)))) (B (C (B * (B zro I))) (B fst I)))) I) I)) (C (B (S (B S (B (B mkPair) ...
```
Here's the standard gradient descend.

EvalMInterLang is the interpreter that evaluate the AST in MetaLanguage(Scala).

We can also evaluate it in ObjectLanguage(DDF) but we dont need that.
```scala
scala> for (i <- Range(0, 50)) {
     |   for (_ <- Range(0, i + 1)) {
     |     weight -= 0.01 * train_it(InterLangTermLang)[NoInfo, Lambda[X => X]](EvalMInterLang)((weight, 1))._2
     |   }
     |   println(weight)
     | }
0.96
4.341977241210556
3.6257877790117097
3.6788539478424824
4.237887042499631
4.213205094493444
3.781694569325477
3.8037766663312405
4.160580115460085
4.148023029578347
3.850379859528939
3.861532825426316
4.119508435779999
4.112216927325566
3.887058545637487
3.893645223931178
4.09474181733468
4.090040491696637
3.9095547577153473
3.913867795693369
4.07833091370856
4.075065969176611
3.9246771929694555
3.927709059513143
4.066703967114532
4.064310977783456
3.9355117699481887
3.9377549841836053
4.05805278249714
4.056226394353731
3.9436438843727495
3.9453687333699117
4.0513722274961745
4.049933832109034
3.949967060769861
3.9513336190237003
4.046061549055351
4.0449000650297275
3.9550216988853295
3.9561305723068863
4.041740545344079
4.04078340687544
3.959153227306749
3.960070722868248
4.038157253866512
4.0373551137313095
3.962592494019389
3.9633640423727083
4.035138266532345
4.034456428573016
```
As you can see, weight approach 4, because 4 * 4 + 2 * 4 + 3 = 27.

Note: Weights are printed with nonconstant iteration between them,
so initially(where they change quicker) more weight will be shown.
