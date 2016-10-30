package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, Loss, LossMatch, LossInfo}

trait BEvalListInfo extends ListInfo[LossInfo, BEval] with BEvalArrowInfo {
  def lLoss[A](l: scala.List[Loss[A]])(implicit ai: LossInfo[A]): Loss[scala.List[A]] =
    new Loss[scala.List[A]] {
      override val tm: LossInfo.Aux[scala.List[A], scala.List[Loss[A]]] = listInfo[A]

      override val tmr: tm.loss = l
    }

  def lloss[A]: Loss[scala.List[A]] => scala.List[Loss[A]] = l => l.get(listInfo(listElmInfo(l.tm)))

  case class ListLC[A]() extends LossMatch[scala.List[A]] {
    override type ret = LossInfo[A]
  }

  case class ListBEC[A]() extends BEvalMatch[scala.List[A]] {
    override type ret = scala.List[BEval[A]]
  }

  def leval[A](e: BEval[scala.List[A]]): scala.List[BEval[A]] = e.get(ListBEC())

  def listEval[A](l: scala.List[BEval[A]])(implicit ai: LossInfo[A]): BEval[scala.List[A]] = new BEval[scala.List[A]] {
    override val tmr: tm.ret = l

    override def eval: scala.List[A] = l.map(_.eval)

    override val tm: BEvalMatch.Aux[scala.List[A], scala.List[BEval[A]]] = ListBEC()

    override val loss: LossInfo[scala.List[A]] = listInfo(ai)
  }

  override implicit def listInfo[A](implicit ai: LossInfo[A]): LossInfo.Aux[scala.List[A], scala.List[Loss[A]]] =
    new LossInfo[scala.List[A]] {
      override def convert: scala.List[A] => BEval[scala.List[A]] = la => listEval[A](la.map(ai.convert))

      override val tm: LossMatch.Aux[scala.List[A], LossInfo[A]] = ListLC()

      override val tmr: tm.ret = ai

      override type ret = scala.List[Loss[A]]

      override def m: CommutativeMonoid[loss] = new CommutativeMonoid[loss] {
        override def zero: loss = scala.List()

        override def append(f1: loss, f2: => loss): loss =

          if (f1.length > f2.length) append(f2, f1)

          else f1.zip(f2).map(p => ai.lm.append(p._1, p._2)) ++ f2.drop(f1.length)
      }

      override def update(x: scala.List[A])(rate: Double)(l: loss): scala.List[A] =
        x.zip(l).map(p => ai.updatel(p._1)(rate)(p._2))
    }

  override def listElmInfo[A]: LossInfo[scala.List[A]] => LossInfo[A] = _.get(ListLC[A]())
}

object BEvalListInfo {
  implicit def apply: BEvalListInfo = new BEvalListInfo {}
}