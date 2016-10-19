package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, Loss, LossCase}
import scalaz.Leibniz.witness

trait BEvalListInfo extends ListInfo[Loss, BEval] with BEvalArrowInfo {
  case class ListLC[A]() extends LossCase[List[A]] {
    override type ret = Loss[A]
  }

  case class ListDEC[A]() extends BEvalCase[List[A]] {
    override type ret = List[BEval[A]]
  }

  def leval[A](e: BEval[List[A]]): List[BEval[A]] = witness(e.ec.unique(ListDEC[A]()))(e.eca)

  def listEval[A](l: List[BEval[A]])(implicit ai: Loss[A]): BEval[List[A]] = new BEval[List[A]] {
    override def eca: ec.ret = l

    override def eval: List[A] = l.map(_.eval)

    override val ec: BEvalCase.Aux[List[A], List[BEval[A]]] = ListDEC()

    override val loss: Loss[List[A]] = listInfo(ai)
  }

  override implicit def listInfo[A](implicit ai: Loss[A]): Loss.Aux[List[A], List[ai.loss]] = new Loss[List[A]] {

    override def convert: List[A] => BEval[List[A]] = la => listEval[A](la.map(ai.convert))

    override val lc: LossCase.Aux[List[A], Loss[A]] = ListLC()

    override def lca: lc.ret = ai

    override type ret = List[ai.loss]

    override def m: CommutativeMonoid[loss] = new CommutativeMonoid[loss] {
      override def zero: loss = scala.List()

      override def append(f1: loss, f2: => loss): loss =
        if (f1.length > f2.length) append(f2, f1)
        else f1.zip(f2).map(p => ai.m.append(p._1, p._2)) ++ f2.drop(f1.length)
    }

    override def update(x: List[A])(rate: Double)(l: loss): List[A] = x.zip(l).map(p => ai.update(p._1)(rate)(p._2))
  }

  override def listElmInfo[A](implicit lai: Loss[List[A]]): Loss[A] = witness(lai.lc.unique(ListLC[A]()))(lai.lca)
}

object BEvalListInfo {
  implicit def apply: BEvalListInfo = new BEvalListInfo {}
}