package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow._
import com.thoughtworks.DDF.{Eval, Loss}

trait EvalComb extends EvalArrow with Comb[Loss, Eval] {
  override def S[A, B, C](implicit at: Loss[A], bt: Loss[B], ct: Loss[C]): Eval[(A => B => C) => (A => B) => A => C] =
    arrowEval[
      A => B => C,
      (A => B) => A => C,
      ArrowLoss[A, ArrowLoss[B, ct.loss]],
      ArrowLoss[A => B, ArrowLoss[A, ct.loss]]](
      abc => (arrowEval[A => B, A => C, ArrowLoss[A, bt.loss], ArrowLoss[A, ct.loss]](
        ab => (arrowEval[A, C, at.loss, ct.loss](a => {
          val bc = aeval(abc).forward(a)
          val b = aeval(ab).forward(a)
          val c = aeval(bc.eb).forward(b.eb)
          (c.eb, l => at.m.append(bc.backward(ArrowLoss(Seq((b.eb, l)))), b.backward(c.backward(l))))
        })(at, ct), l => ArrowLoss(l.seq.map(x => (x._1, {
          val bc = aeval(abc).forward(x._1)
          val b = aeval(ab).forward(x._1)
          val c = aeval(bc.eb).forward(b.eb)
          c.backward(x._2)
        }))))), l => ArrowLoss(l.seq.flatMap(x => x._2.seq.map(y => (
        y._1, ArrowLoss(Seq((app(x._1)(y._1), y._2)))))))))

  override def K[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => B => A] =
    arrowEval[A, B => A, at.loss, ArrowLoss[B, at.loss]](a => (
      arrowEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
      l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, ArrowInfo(bt, at))

  override def I[A](implicit at: Loss[A]): Eval[A => A] = arrowEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)

  override def Y[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A => B) => A => B) => A => B] = {
    type abt = ArrowLoss[A, bt.loss]
    arrowEval[(A => B) => A => B, A => B, ArrowLoss[A => B, abt], abt](abab => {
      val ab = arrowEval[A, B, at.loss, bt.loss](a => {
        val fa = aeval(app(abab)(app(Y[A, B])(abab))).forward(a)(at, bt)
        (fa.eb, fa.backward)
      })(at, bt)
      (ab, abl => ArrowLoss(abl.seq.map(p => (ab, ArrowLoss(Seq(p))))))
    })
  }


  override def B[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(B => C) => (A => B) => A => C] =
    arrowEval[B => C, (A => B) => A => C, ArrowLoss[B, ci.loss], ArrowLoss[A => B, ArrowLoss[A, ci.loss]]](bc =>
      (arrowEval[A => B, A => C, ArrowLoss[A, bi.loss], ArrowLoss[A, ci.loss]](ab => (arrowEval[A, C, ai.loss, ci.loss](a => {
        val b = aeval(ab).forward(a)
        val c = aeval(bc).forward(b.eb)
        (c.eb, l => b.backward(c.backward(l)))
      })(ai, ci), l => ArrowLoss(l.seq.map(x => {
        val b = aeval(ab).forward(x._1)
        val c = aeval(bc).forward(b.eb)
        (x._1, c.backward(x._2))
      })))), l => ArrowLoss(l.seq.flatMap(x => x._2.seq.map(y => (app(x._1)(y._1), y._2))))))

  override def C[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(A => B => C) => B => A => C] =
    arrowEval[A => B => C, B => A => C, ArrowLoss[A, ArrowLoss[B, ci.loss]], ArrowLoss[B, ArrowLoss[A, ci.loss]]](abc =>
      (arrowEval[B, A => C, bi.loss, ArrowLoss[A, ci.loss]](b =>
        (arrowEval[A, C, ai.loss, ci.loss](a => {
          val bc = aeval(abc).forward(a)
          val c = aeval(bc.eb).forward(b)
          (c.eb, l => bc.backward(ArrowLoss(Seq((b, l)))))
        })(ai, ci), l => l.seq.map(p => aeval(aeval(abc).forward(p._1).eb).forward(b).backward(p._2)).
          foldRight(bi.m.zero)((l, r) => bi.m.append(l, r))))(bi, ArrowInfo(ai, ci)), l =>
        ArrowLoss(l.seq.flatMap(b => b._2.seq.map(a =>
          (a._1, ArrowLoss(Seq((b._1, a._2)))))))))

  override def W[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[(A => A => B) => A => B] =
    arrowEval[A => A => B, A => B, ArrowLoss[A, ArrowLoss[A, bi.loss]], ArrowLoss[A, bi.loss]](aab =>
      (arrowEval[A, B, ai.loss, bi.loss](a => {
        val ab = aeval(aab).forward(a)
        val b = aeval(ab.eb).forward(a)
        (b.eb, bl => ai.m.append(b.backward(bl), ab.backward(ArrowLoss(Seq((a, bl))))))
      })(ai, bi), l => ArrowLoss(l.seq.map(x => (x._1, ArrowLoss(Seq((x._1, x._2))))))))
}

object EvalComb {
  implicit def apply = new EvalComb {}
}
