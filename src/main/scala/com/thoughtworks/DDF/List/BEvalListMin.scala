package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArr}
import com.thoughtworks.DDF.Combinators.BEvalComb
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalListMin extends ListMin[LossInfo, BEval] with BEvalListInfo with BEvalArr {
  override def nil[A](implicit ai: LossInfo[A]): BEval[scala.List[A]] = listEval(scala.List())

  override def cons[A](implicit ai: LossInfo[A]): BEval[A => scala.List[A] => scala.List[A]] =
    aEval[A, scala.List[A] => scala.List[A]](a =>
      (aEval[scala.List[A], scala.List[A]](la =>
        (listEval(a :: leval(la)), l =>
          lLoss(lloss(l).tail))), x => aloss(x).mapReduce(_ => l => lloss(l).head)(ai.lm)))(
      ai, aInfo(listInfo(ai), listInfo(ai)))

  private def comb = BEvalComb.apply

  override def listMatch[A, B](implicit ai: LossInfo[A], bi: LossInfo[B]):
  BEval[scala.List[A] => B => (A => scala.List[A] => B) => B] =
    aEval[scala.List[A], B => (A => scala.List[A] => B) => B](l => leval(l) match {
      case Nil => (comb.K[B, A => scala.List[A] => B], l => listInfo(ai).lm.zero)
      case lh :: lt => (comb.K_(comb.C__(comb.C__(comb.I[A => scala.List[A] => B])(lh))(listEval(lt))),
        x => aloss(x).mapReduce(b => y => aloss(y).mapReduce(alab => l => {
          val lab = aeval(alab).forward(lh)
          val b = aeval(lab.eb).forward(listEval(lt))
          lLoss(lab.backward(lossA(listEval(lt))(l)) +: lloss(b.backward(l)))
        })(listInfo(ai).lm))(listInfo(ai).lm))
    })
}

object BEvalListMin {
  implicit def apply: BEvalListMin = new BEvalListMin {}
}