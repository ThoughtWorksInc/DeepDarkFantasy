package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow._
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalComb extends BEvalArrow with Comb[Loss, BEval] {
  override def S[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): BEval[(A => B => C) => (A => B) => A => C] =
    arrowEval[
      A => B => C,
      (A => B) => A => C,
      ArrowLoss[A, ArrowLoss[B, ci.loss]],
      ArrowLoss[A => B, ArrowLoss[A, ci.loss]]](
      abc => (arrowEval[A => B, A => C, ArrowLoss[A, bi.loss], ArrowLoss[A, ci.loss]](
        ab => (arrowEval[A, C, ai.loss, ci.loss](a => {
          val bc = aBEval(abc).forward(a)
          val b = aBEval(ab).forward(a)
          val c = aBEval(bc.eb).forward(b.eb)
          (c.eb, l => ai.m.append(bc.backward(ArrowLoss(b.eb)(l)), b.backward(c.backward(l))))
        })(ai, ci), _.mapReduce[ArrowLoss[A, bi.loss]](a => l => {
          val bc = aBEval(abc).forward(a)
          val b = aBEval(ab).forward(a)
          val c = aBEval(bc.eb).forward(b.eb)
          ArrowLoss[A, bi.loss](a)(c.backward(l))
        })(arrowInfo(ai, bi).m))),
        _.mapReduce(ab => _.mapReduce(a => l => ArrowLoss(a)(ArrowLoss(aBEval(ab).forward(a).eb)(l)))(
          arrowInfo(ai, arrowInfo(bi, ci)).m))(
          arrowInfo(ai, arrowInfo(bi, ci)).m)))

  override def K[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[A => B => A] =
    arrowEval[A, B => A, ai.loss, ArrowLoss[B, ai.loss]](a => (
      arrowEval[B, A, bi.loss, ai.loss](_ => (a, _ => bi.m.zero))(bi, ai),
      _.mapReduce(_ => l => l)(ai.m)))(ai, arrowInfo(bi, ai))

  override def I[A](implicit ai: Loss[A]): BEval[A => A] = arrowEval[A, A, ai.loss, ai.loss](x => (x, y => y))(ai, ai)

  override def Y[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[((A => B) => A => B) => A => B] = {
    type abi = ArrowLoss[A, bi.loss]
    arrowEval[(A => B) => A => B, A => B, ArrowLoss[A => B, abi], abi](abab => {
      val ab = arrowEval[A, B, ai.loss, bi.loss](a => {
        val fa = aBEval(app(abab)(app(Y[A, B])(abab))).forward(a)(ai, bi)
        (fa.eb, fa.backward)
      })(ai, bi)
      (ab, _.mapReduce(a => l => ArrowLoss(ab)(ArrowLoss(a)(l)))(arrowInfo(arrowInfo(ai, bi), arrowInfo(ai, bi)).m))
    })
  }

  override def B[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): BEval[(B => C) => (A => B) => A => C] =
    arrowEval[B => C, (A => B) => A => C, ArrowLoss[B, ci.loss], ArrowLoss[A => B, ArrowLoss[A, ci.loss]]](bc =>
      (arrowEval[A => B, A => C, ArrowLoss[A, bi.loss], ArrowLoss[A, ci.loss]](ab => (arrowEval[A, C, ai.loss, ci.loss](a => {
        val b = aBEval(ab).forward(a)
        val c = aBEval(bc).forward(b.eb)
        (c.eb, l => b.backward(c.backward(l)))
      })(ai, ci), _.mapReduce(a => l => {
        val b = aBEval(ab).forward(a)
        val c = aBEval(bc).forward(b.eb)
        ArrowLoss(a)(c.backward(l))
      })(arrowInfo(ai, bi).m))), _.mapReduce(ab => _.mapReduce(a => l => {
        val b = aBEval(ab).forward(a)
        ArrowLoss(b.eb)(l)
      })(arrowInfo(bi, ci).m))(arrowInfo(bi, ci).m)))

  override def C[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): BEval[(A => B => C) => B => A => C] =
    arrowEval[A => B => C, B => A => C, ArrowLoss[A, ArrowLoss[B, ci.loss]], ArrowLoss[B, ArrowLoss[A, ci.loss]]](abc =>
      (arrowEval[B, A => C, bi.loss, ArrowLoss[A, ci.loss]](b =>
        (arrowEval[A, C, ai.loss, ci.loss](a => {
          val bc = aBEval(abc).forward(a)
          val c = aBEval(bc.eb).forward(b)
          (c.eb, l => bc.backward(ArrowLoss(b)(l)))
        })(ai, ci), _.mapReduce(a => aBEval(aBEval(abc).forward(a).eb).forward(b).backward)(bi.m)))(bi, arrowInfo(ai, ci)),
        _.mapReduce(b => _.mapReduce(a => l => ArrowLoss(a)(ArrowLoss(b)(l)))(
          arrowInfo(ai, arrowInfo(bi, ci)).m))(
          arrowInfo(ai, arrowInfo(bi, ci)).m)))

  override def W[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[(A => A => B) => A => B] =
    arrowEval[A => A => B, A => B, ArrowLoss[A, ArrowLoss[A, bi.loss]], ArrowLoss[A, bi.loss]](aab =>
      (arrowEval[A, B, ai.loss, bi.loss](a => {
        val ab = aBEval(aab).forward(a)
        val b = aBEval(ab.eb).forward(a)
        (b.eb, bl => ai.m.append(b.backward(bl), ab.backward(ArrowLoss(a)(bl))))
      })(ai, bi), _.mapReduce(a => l => ArrowLoss(a)(ArrowLoss(a)(l)))(arrowInfo(ai, arrowInfo(ai, bi)).m)))

  override def Let[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[A => (A => B) => B] = app(C[A => B, A, B])(App)

  override def App[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[(A => B) => A => B] = I[A => B]
}

object BEvalComb {
  implicit def apply = new BEvalComb {}
}
