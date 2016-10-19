package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.BEvalComb
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalListMin extends ListMin[Loss, BEval] with BEvalListInfo with BEvalArrow {
  override def nil[A](implicit ai: Loss[A]): BEval[List[A]] = listEval(scala.List())

  override def cons[A](implicit ai: Loss[A]): BEval[A => List[A] => List[A]] =
    arrowEval[A, List[A] => List[A], ai.loss, ArrowLoss[List[A], List[ai.loss]]](a =>
      (arrowEval[List[A], List[A], List[ai.loss], List[ai.loss]](la =>
        (listEval(a :: leval(la)), l => l.tail)), _.mapReduce(_ => l => l.head)(ai.m)))(
      ai, arrowInfo(listInfo(ai), listInfo(ai)))

  private def comb = BEvalComb.apply

  override def listMatch[A, B](implicit ai: Loss[A], bi: Loss[B]): BEval[List[A] => B => (A => List[A] => B) => B] =
    arrowEval[
      List[A],
      B => (A => List[A] => B) => B,
      List[ai.loss],
      ArrowLoss[B, ArrowLoss[A => List[A] => B, bi.loss]]](l => leval(l) match {
      case Nil => (comb.K[B, A => List[A] => B], l => List())
      case lh :: lt => (app(comb.K[(A => List[A] => B) => B, B])(app(app(comb.C[A => List[A] => B, List[A], B])(app(app(
        comb.C[A => List[A] => B, A, List[A] => B])(
        comb.I[A => List[A] => B]))(lh)))(listEval(lt))),
        _.mapReduce(b => _.mapReduce(alab => l => {
          val lab = aeval(alab).forward(lh)
          val b = aeval(lab.eb).forward(listEval(lt))
          lab.backward(ArrowLoss(listEval(lt))(l)) +: b.backward(l)
        })(listInfo(ai).m))(listInfo(ai).m))
    })
}

object BEvalListMin {
  implicit def apply: BEvalListMin = new BEvalListMin {}
}