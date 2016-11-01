package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.BEvalArr
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalProdMin extends ProdMin[LossInfo, BEval] with BEvalProdInfo with BEvalArr {
  override def zro[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((A, B)) => A] =
    aEval[(A, B), A](p => (peval(p)._1, al => lossP(al)(bi.lm.zero)))(prodInfo(ai, bi), ai)

  override def fst[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[((A, B)) => B] =
    aEval[(A, B), B](p => (peval(p)._2, bl => lossP(ai.lm.zero)(bl)))(prodInfo(ai, bi), bi)

  override def mkProduct[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]): BEval[(A) => (B) => (A, B)] =
    aEval[A, B => (A, B)](a => (aEval[B, (A, B)](b =>
        (productEval(a, b), ploss1))(bi, prodInfo(ai, bi)),
        x => aloss(x).mapReduce(_ => ploss0)(ai.lm)))(
      ai, aInfo(bi, prodInfo(ai, bi)))
}

object BEvalProdMin {
  implicit def apply = new BEvalProdMin {}
}