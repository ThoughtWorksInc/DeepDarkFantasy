package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow._
import com.thoughtworks.DDF.{BEval, Loss, LossInfo}

trait BEvalComb extends BEvalArrow with Comb[LossInfo, BEval] {
  override def S[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]): BEval[(A => B => C) => (A => B) => A => C] =
    aEval[
      A => B => C,
      (A => B) => A => C](
      abc => (aEval[A => B, A => C](
        ab => (aEval[A, C](a => {
          val bc = aeval(abc).forward(a)
          val b = aeval(ab).forward(a)
          val c = aeval(bc.eb).forward(b.eb)
          (c.eb, l => ai.lm.append(bc.backward(lossA(b.eb)(l)), b.backward(c.backward(l))))
        })(ai, ci), x => aloss(x).mapReduce[Loss[A => B]](a => l => {
          val bc = aeval(abc).forward(a)
          val b = aeval(ab).forward(a)
          val c = aeval(bc.eb).forward(b.eb)
          lossA[A, B](a)(c.backward(l))
        })(aInfo(ai, bi).lm))),
        x => aloss(x).mapReduce(ab => y => aloss(y).mapReduce(a => l => lossA(a)(lossA(aeval(ab).forward(a).eb)(l)))(
          aInfo(ai, aInfo(bi, ci)).lm))(
          aInfo(ai, aInfo(bi, ci)).lm)))

  override def K[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[A => B => A] =
    aEval[A, B => A](a => (
      aEval[B, A](_ => (a, _ => bi.lm.zero))(bi, ai),
      x => aloss(x).mapReduce(_ => l => l)(ai.lm)))(ai, aInfo(bi, ai))

  override def I[A](implicit ai: LossInfo[A]): BEval[A => A] = aEval[A, A](x => (x, y => y))(ai, ai)

  override def Y[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((A => B) => A => B) => A => B] = {
    type abi = ArrowLoss[A, bi.loss]
    aEval[(A => B) => A => B, A => B](abab => {
      val ab = aEval[A, B](a => {
        val fa = aeval(app(abab)(app(Y[A, B])(abab))).forward(a)
        (fa.eb, fa.backward)
      })(ai, bi)
      (ab, x => aloss(x).mapReduce(a => l => lossA(ab)(lossA(a)(l)))(
        aInfo(aInfo(ai, bi), aInfo(ai, bi)).lm))
    })
  }

  override def B[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]): BEval[(B => C) => (A => B) => A => C] =
    aEval[B => C, (A => B) => A => C](bc =>
      (aEval[A => B, A => C](ab => (aEval[A, C](a => {
        val b = aeval(ab).forward(a)
        val c = aeval(bc).forward(b.eb)
        (c.eb, l => b.backward(c.backward(l)))
      })(ai, ci), x => aloss(x).mapReduce(a => l => {
        val b = aeval(ab).forward(a)
        val c = aeval(bc).forward(b.eb)
        lossA(a)(c.backward(l))
      })(aInfo(ai, bi).lm))), x => aloss(x).mapReduce(ab => y => aloss(y).mapReduce(a => l => {
        val b = aeval(ab).forward(a)
        lossA(b.eb)(l)
      })(aInfo(bi, ci).lm))(aInfo(bi, ci).lm)))

  override def C[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]): BEval[(A => B => C) => B => A => C] =
    aEval[A => B => C, B => A => C](abc =>
      (aEval[B, A => C](b =>
        (aEval[A, C](a => {
          val bc = aeval(abc).forward(a)
          val c = aeval(bc.eb).forward(b)
          (c.eb, l => bc.backward(lossA(b)(l)))
        })(ai, ci), x => aloss(x).mapReduce(a => aeval(aeval(abc).forward(a).eb).forward(b).backward)(bi.lm))),
        x => aloss(x).mapReduce(b => y => aloss(y).mapReduce(a => l => lossA(a)(lossA(b)(l)))(
          aInfo(ai, aInfo(bi, ci)).lm))(
          aInfo(ai, aInfo(bi, ci)).lm)))

  override def W[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => A => B) => A => B] =
    aEval[A => A => B, A => B](aab =>
      (aEval[A, B](a => {
        val ab = aeval(aab).forward(a)
        val b = aeval(ab.eb).forward(a)
        (b.eb, bl => ai.lm.append(b.backward(bl), ab.backward(lossA(a)(bl))))
      })(ai, bi), x => aloss(x).mapReduce(a => l => lossA(a)(lossA(a)(l)))(aInfo(ai, aInfo(ai, bi)).lm)))

  override def Let[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[A => (A => B) => B] =
    app(C[A => B, A, B])(App)

  override def App[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A => B) => A => B] = I[A => B]
}

object BEvalComb {
  implicit def apply = new BEvalComb {}
}
