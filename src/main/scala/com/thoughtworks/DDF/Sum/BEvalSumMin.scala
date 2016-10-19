package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.{BEvalComb, Comb}
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalSumMin extends SumMin[Loss, BEval] with BEvalSumInfo with BEvalArrow {
  override def left[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[A => Either[A, B]] =
    arrowEval[A, Either[A, B], ai.loss, (ai.loss, bi.loss)](ea => (sumEval(scala.Left(ea)), _._1))(ai, sumInfo(ai, bi))

  override def right[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[B => Either[A, B]] =
    arrowEval[B, Either[A, B], bi.loss, (ai.loss, bi.loss)](eb => (sumEval(scala.Right(eb)), _._2))(bi, sumInfo(ai, bi))

  private val comb: Comb[Loss, BEval] = BEvalComb.apply

  override def sumMatch[A, B, C](implicit ai: Loss[A], bi: Loss[B], ci: Loss[C]):
  BEval[Either[A, B] => (A => C) => (B => C) => C] =
    arrowEval[
      Either[A, B],
      (A => C) => (B => C) => C,
      (ai.loss, bi.loss),
      ArrowLoss[A => C, ArrowLoss[B => C, ci.loss]]](sum => seval(sum) match {
      case Left(a) =>
        (comb.app(comb.C[B => C, A => C, C])(comb.app(comb.K[(A => C) => C, B => C])(comb.app(comb.Let[A, C])(a))),
          _.mapReduce(ac => _.mapReduce(bc => l => (aeval(ac).forward(a).backward(l), bi.m.zero))(
            sumInfo(ai, bi).m))(sumInfo(ai, bi).m))
      case Right(b) =>
        (comb.app(comb.K[(B => C) => C, A => C])(comb.app(comb.Let[B, C])(b)),
          _.mapReduce(ac => _.mapReduce(bc => l => (ai.m.zero, aeval(bc).forward(b).backward(l)))(
            sumInfo(ai, bi).m))(sumInfo(ai, bi).m))
    })
}

object BEvalSumMin {
  implicit def apply: BEvalSumMin = new BEvalSumMin {}
}
