package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.Combinators.{BEvalComb, Comb}
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalSumMin extends SumMin[LossInfo, BEval] with BEvalSumInfo with BEvalArrow {
  override def left[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[A => Either[A, B]] =
    aEval[A, Either[A, B]](ea => (sumEval(scala.Left(ea)), slossl))(ai, sumInfo(ai, bi))

  override def right[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[B => Either[A, B]] =
    aEval[B, Either[A, B]](eb => (sumEval(scala.Right(eb)), slossr))(bi, sumInfo(ai, bi))

  private val comb: Comb[LossInfo, BEval] = BEvalComb.apply

  override def sumMatch[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[Either[A, B] => (A => C) => (B => C) => C] =
    aEval[Either[A, B], (A => C) => (B => C) => C](sum => seval(sum) match {
      case Left(a) =>
        (comb.C_[B => C, A => C, C](comb.K_[(A => C) => C, B => C](comb.Let_[A, C](a))),
          x => aloss(x).mapReduce(ac => y =>
            aloss(y).mapReduce(bc => l => lossS(aeval(ac).forward(a).backward(l))(bi.lm.zero))(
            sumInfo(ai, bi).lm))(sumInfo(ai, bi).lm))
      case Right(b) =>
        (comb.K_[(B => C) => C, A => C](comb.Let_[B, C](b)),
          x => aloss(x).mapReduce(ac => y =>
            aloss(y).mapReduce(bc => l => lossS(ai.lm.zero)(aeval(bc).forward(b).backward(l)))(
            sumInfo(ai, bi).lm))(sumInfo(ai, bi).lm))
    })
}

object BEvalSumMin {
  implicit def apply: BEvalSumMin = new BEvalSumMin {}
}
