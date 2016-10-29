package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalProduct extends Product[LossInfo, BEval] with BEvalProductMin with BEvalArrow {
  def curry[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]): BEval[(((A, B)) => C) => A => B => C] =
    arrowEval[((A, B)) => C, A => B => C, ArrowLoss[(A, B), ci.loss], ArrowLoss[A, ArrowLoss[B, ci.loss]]](abc =>
      (arrowEval[A, B => C, ai.loss, ArrowLoss[B, ci.loss]](a =>
        (arrowEval[B, C, bi.loss, ci.loss](b => {
          val c = aeval(abc).forward(productEval(a, b))
          (c.eb, l => c.backward(l)._2)
        })(bi, ci), l => l.mapReduce(b => l => {
          val c = aeval(abc).forward(productEval(a, b))
          c.backward(l)._1
        })(ai.m)))
        (ai, arrowInfo(bi, ci)),
        _.mapReduce(a => _.mapReduce(b => l => ArrowLoss(productEval(a, b))(l))(
          arrowInfo(productInfo(ai, bi), ci).m))(
          arrowInfo(productInfo(ai, bi), ci).m)))

  def uncurry[A, B, C](implicit ai: LossInfo[A], bi: LossInfo[B], ci: LossInfo[C]): BEval[(A => B => C) => ((A, B)) => C] =
    arrowEval[A => B => C, ((A, B)) => C, ArrowLoss[A, ArrowLoss[B, ci.loss]], ArrowLoss[(A, B), ci.loss]](abc =>
      (arrowEval[(A, B), C, (ai.loss, bi.loss), ci.loss](ab => {
        val bc = aeval(abc).forward(peval(ab)._1)
        val c = aeval(bc.eb).forward(peval(ab)._2)
        (c.eb, l => (bc.backward(ArrowLoss(peval(ab)._2)(l)), c.backward(l)))
      })(productInfo(ai, bi), ci),
        _.mapReduce(p => l => ArrowLoss(peval(p)._1)(ArrowLoss(peval(p)._2)(l)))(arrowInfo(ai, arrowInfo(bi, ci)).m)))
}

object BEvalProduct {
  implicit def apply = new BEvalProduct {}
}