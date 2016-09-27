package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arr._
import com.thoughtworks.DDF.Eval._

trait CombEval extends Comb[Loss, Eval] with ArrEval {
  override def S[A, B, C](implicit at: Loss[A], bt: Loss[B], ct: Loss[C]): Eval[(A => B => C) => (A => B) => A => C] =
    arrEval[
      A => B => C,
      (A => B) => A => C,
      ArrLoss[A, ArrLoss[B, ct.loss]],
      ArrLoss[A => B, ArrLoss[A, ct.loss]]](
      abc => (arrEval[A => B, A => C, ArrLoss[A, bt.loss], ArrLoss[A, ct.loss]](
        ab => (arrEval[A, C, at.loss, ct.loss](a => {
          val bc = aeval(abc).forward(a)
          val b = aeval(ab).forward(a)
          val c = aeval(bc.eb).forward(b.eb)
          (c.eb, l => at.m.append(bc.backward(ArrLoss(Seq((b.eb, l)))), b.backward(c.backward(l))))
        })(at, ct), l => ArrLoss(l.seq.map(x => (x._1, {
          val bc = aeval(abc).forward(x._1)
          val b = aeval(ab).forward(x._1)
          val c = aeval(bc.eb).forward(b.eb)
          c.backward(x._2)
        }))))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (
        y._1, ArrLoss(Seq((app(x._1)(y._1), y._2)))))))))

  override def K[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[A => B => A] =
    arrEval[A, B => A, at.loss, ArrLoss[B, at.loss]](a => (
      arrEval[B, A, bt.loss, at.loss](_ => (a, _ => bt.m.zero))(bt, at),
      l => l.seq.map(_._2).fold(at.m.zero)((l, r) => at.m.append(l, r))))(at, arrLoss(bt, at))

  override def I[A](implicit at: Loss[A]): Eval[A => A] = arrEval[A, A, at.loss, at.loss](x => (x, y => y))(at, at)

  override def Y[A, B](implicit at: Loss[A], bt: Loss[B]): Eval[((A => B) => A => B) => A => B] = {
    type abt = ArrLoss[A, bt.loss]
    arrEval[(A => B) => A => B, A => B, ArrLoss[A => B, abt], abt](abab => {
      val ab = arrEval[A, B, at.loss, bt.loss](a => {
        val fa = aeval(app(abab)(app(Y[A, B])(abab))).forward(a)(at, bt)
        (fa.eb, fa.backward)
      })(at, bt)
      (ab, abl => ArrLoss(abl.seq.map(p => (ab, ArrLoss(Seq(p))))))
    })
  }


  override def B[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(B => C) => (A => B) => A => C] =
    arrEval[B => C, (A => B) => A => C, ArrLoss[B, ci.loss], ArrLoss[A => B, ArrLoss[A, ci.loss]]](bc =>
      (arrEval[A => B, A => C, ArrLoss[A, bi.loss], ArrLoss[A, ci.loss]](ab => (arrEval[A, C, ai.loss, ci.loss](a => {
        val b = aeval(ab).forward(a)
        val c = aeval(bc).forward(b.eb)
        (c.eb, l => b.backward(c.backward(l)))
      })(ai, ci), l => ArrLoss(l.seq.map(x => {
        val b = aeval(ab).forward(x._1)
        val c = aeval(bc).forward(b.eb)
        (x._1, c.backward(x._2))
      })))), l => ArrLoss(l.seq.flatMap(x => x._2.seq.map(y => (app(x._1)(y._1), y._2))))))

  override def C[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]): Eval[(A => B => C) => B => A => C] =
    arrEval[A => B => C, B => A => C, ArrLoss[A, ArrLoss[B, ci.loss]], ArrLoss[B, ArrLoss[A, ci.loss]]](abc =>
      (arrEval[B, A => C, bi.loss, ArrLoss[A, ci.loss]](b =>
        (arrEval[A, C, ai.loss, ci.loss](a => {
          val bc = aeval(abc).forward(a)
          val c = aeval(bc.eb).forward(b)
          (c.eb, l => bc.backward(ArrLoss(Seq((b, l)))))
        })(ai, ci), l => l.seq.map(p => aeval(aeval(abc).forward(p._1).eb).forward(b).backward(p._2)).
          foldRight(bi.m.zero)((l, r) => bi.m.append(l, r))))(bi, arrLoss(ai, ci)), l => ArrLoss(l.seq.flatMap(b => b._2.seq.map(a =>
        (a._1, ArrLoss(Seq((b._1, a._2)))))))))

  override def W[A, B](implicit ai: Loss[A], bi: Loss[B]): Eval[(A => A => B) => A => B] =
    arrEval[A => A => B, A => B, ArrLoss[A, ArrLoss[A, bi.loss]], ArrLoss[A, bi.loss]](aab =>
      (arrEval[A, B, ai.loss, bi.loss](a => {
        val ab = aeval(aab).forward(a)
        val b = aeval(ab.eb).forward(a)
        (b.eb, bl => ai.m.append(b.backward(bl), ab.backward(ArrLoss(Seq((a, bl))))))
      })(ai, bi), l => ArrLoss(l.seq.map(x => (x._1, ArrLoss(Seq((x._1, x._2))))))))
}
