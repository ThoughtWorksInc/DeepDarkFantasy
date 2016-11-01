package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.BEvalArr
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalProd extends Prod[LossInfo, BEval] with BEvalProdMin with BEvalArr {
  def curry[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[(((A, B)) => C) => A => B => C] =
    aEval[((A, B)) => C, A => B => C](abc =>
      (aEval[A, B => C](a =>
        (aEval[B, C](b => {
          val c = aeval(abc).forward(productEval(a, b))
          (c.eb, l => ploss1(c.backward(l)))
        })(bi, ci), l => aloss(l).mapReduce(b => l => {
          val c = aeval(abc).forward(productEval(a, b))
          ploss0(c.backward(l))
        })(ai.lm)))
        (ai, aInfo(bi, ci)),
        x => aloss(x).mapReduce(a => y => aloss(y).mapReduce(b => l => lossA(productEval(a, b))(l))(
          aInfo(prodInfo(ai, bi), ci).lm))(
          aInfo(prodInfo(ai, bi), ci).lm)))

  def uncurry[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]):
  BEval[(A => B => C) => ((A, B)) => C] =
    aEval[A => B => C, ((A, B)) => C](abc =>
      (aEval[(A, B), C](ab => {
        val bc = aeval(abc).forward(peval(ab)._1)
        val c = aeval(bc.eb).forward(peval(ab)._2)
        (c.eb, l => lossP(bc.backward(lossA(peval(ab)._2)(l)))(c.backward(l)))
      })(prodInfo(ai, bi), ci),
        x => aloss(x).mapReduce(p => l => lossA(peval(p)._1)(lossA(peval(p)._2)(l)))(aInfo(ai, aInfo(bi, ci)).lm)))
}

object BEvalProd {
  implicit def apply = new BEvalProd {}
}