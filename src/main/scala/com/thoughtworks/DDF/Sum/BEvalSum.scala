package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalSum extends Sum[LossInfo, BEval] with BEvalSumMin {
  override def sumComm[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[Either[A, B] => Either[B, A]] =
    aEval[Either[A, B], Either[B, A]](s => (sumEval(seval(s).swap), l => lossS(slossr(l))(slossl(l))))

  override def sumAssocLR[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[Either[Either[A, B], C] => Either[A, Either[B, C]]] =
    aEval[Either[Either[A, B], C], Either[A, Either[B, C]]](s =>
      (seval(s) match {
        case Left(l) => seval(l) match {
          case Left(x) => sumEval(Left(x))
          case Right(y) => sumEval(Right(sumEval(Left(y))))
        }
        case Right(r) => sumEval(Right(sumEval(Right(r))))
      },
        l => lossS(lossS(slossl(l))(slossl(slossr(l))))(slossr(slossr(l)))))

  override def sumAssocRL[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[Either[A, Either[B, C]] => Either[Either[A, B], C]] =
    aEval[Either[A, Either[B, C]], Either[Either[A, B], C]](s =>
      (seval(s) match {
        case Left(l) => sumEval(Left(sumEval(Left(l))))
        case Right(r) => seval(r) match {
          case Left(x) => sumEval(Left(sumEval(Right(x))))
          case Right(y) => sumEval(Right(y))
        }
      }, l => lossS(slossl(slossl(l)))(lossS(slossr(slossl(l)))(slossr(l)))))
}

object BEvalSum {
  implicit def apply: BEvalSum = new BEvalSum {}
}