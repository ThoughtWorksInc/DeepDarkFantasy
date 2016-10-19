package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalProductMin extends ProductMin[Loss, BEval] with BEvalProductInfo with BEvalArrow {
  override def zeroth[A, B](implicit at: Loss[A], bt: Loss[B]): BEval[((A, B)) => A] =
    arrowEval[(A, B), A, (at.loss, bt.loss), at.loss](p => (peval(p)._1, al => (al, bt.m.zero)))(productInfo(at, bt), at)

  override def first[A, B](implicit at: Loss[A], bt: Loss[B]): BEval[((A, B)) => B] =
    arrowEval[(A, B), B, (at.loss, bt.loss), bt.loss](p => (peval(p)._2, bl => (at.m.zero, bl)))(productInfo(at, bt), bt)

  override def mkProduct[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[(A) => (B) => (A, B)] =
    arrowEval[A, B => (A, B), ai.loss, ArrowLoss[B, (ai.loss, bi.loss)]](a =>
      (arrowEval[B, (A, B), bi.loss, (ai.loss, bi.loss)](b =>
        (productEval(a, b), _._2))(bi, productInfo(ai, bi)),
        _.mapReduce(_ => _._1)(ai.m)))(
      ai, arrowInfo(bi, productInfo(ai, bi)))
}

object BEvalProductMin {
  implicit def apply = new BEvalProductMin {}
}