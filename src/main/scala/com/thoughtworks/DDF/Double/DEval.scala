package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF._

import scalaz.Leibniz._
import scalaz.Monoid

trait DEval extends DLang[Loss, Eval] {
  def dEval(d: Double) = new Eval[Double] {
    override val loss: Loss[Double] = dLoss

    override def eval: Double = d

    override val ec: EvalCase.Aux[Double, Double] = DEC

    override def eca: ec.ret = d
  }

  implicit def dLoss: Loss.Aux[Double, DLoss] = new Loss[Double] {
    override def m: Monoid[DLoss] = new Monoid[DLoss] {
      override def zero: DLoss = DLoss(0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.d + f2.d)
    }

    override def conv: Double => Eval[Double] = dEval

    override val lc: LossCase.Aux[Double, DLC.ret] = DLC

    override def lca: lc.ret = ()

    override type ret = DLoss
  }

  def deval(d: Eval[Double]): Double = witness(d.ec.unique(DEC))(d.eca)
}
