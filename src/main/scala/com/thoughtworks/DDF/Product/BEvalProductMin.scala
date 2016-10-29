package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalProductMin extends ProductMin[LossInfo, BEval] with BEvalProductInfo with BEvalArrow {
  override def zeroth[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((A, B)) => A] =
    aEval[(A, B), A](p => (peval(p)._1, al => lossP(al)(bi.lm.zero)))(productInfo(ai, bi), ai)

  override def first[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((A, B)) => B] =
    aEval[(A, B), B](p => (peval(p)._2, bl => lossP(ai.lm.zero)(bl)))(productInfo(ai, bi), bi)

  override def mkProduct[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A) => (B) => (A, B)] =
    aEval[A, B => (A, B)](a => (aEval[B, (A, B)](b =>
        (productEval(a, b), ploss1))(bi, productInfo(ai, bi)),
        x => aloss(x).mapReduce(_ => ploss0)(ai.lm)))(
      ai, aInfo(bi, productInfo(ai, bi)))
}

object BEvalProductMin {
  implicit def apply = new BEvalProductMin {}
}