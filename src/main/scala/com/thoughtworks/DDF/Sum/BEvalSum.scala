package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalSum extends Sum[LossInfo, BEval] with BEvalSumMin {
  override def sumComm[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[Either[A, B] => Either[B, A]] =
    arrowEval[Either[A, B], Either[B, A], (ai.loss, bi.loss), (bi.loss, ai.loss)](s =>
      (sumEval(seval(s).swap), l => (l._2, l._1)))

  override def sumAssocLR[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[Either[Either[A, B], C] => Either[A, Either[B, C]]] =
    arrowEval[
      Either[Either[A, B], C],
      Either[A, Either[B, C]],
      ((ai.loss, bi.loss), ci.loss),
      (ai.loss, (bi.loss, ci.loss))](s =>
      (seval(s) match {
        case Left(l) => seval(l) match {
          case Left(x) => sumEval(Left(x))
          case Right(y) => sumEval(Right(sumEval(Left(y))))
        }
        case Right(r) => sumEval(Right(sumEval(Right(r))))
      },
        l => ((l._1, l._2._1), l._2._2)))

  override def sumAssocRL[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[Either[A, Either[B, C]] => Either[Either[A, B], C]] =
    arrowEval[
      Either[A, Either[B, C]],
      Either[Either[A, B], C],
      (ai.loss, (bi.loss, ci.loss)),
      ((ai.loss, bi.loss), ci.loss)](s =>
      (seval(s) match {
        case Left(l) => sumEval(Left(sumEval(Left(l))))
        case Right(r) => seval(r) match {
          case Left(x) => sumEval(Left(sumEval(Right(x))))
          case Right(y) => sumEval(Right(y))
        }
      }, l => (l._1._1, (l._1._2, l._2))))
}

object BEvalSum {
  implicit def apply: BEvalSum = new BEvalSum {}
}