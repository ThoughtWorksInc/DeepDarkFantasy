package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.{EvalArrow, ArrowLoss}
import com.thoughtworks.DDF._
import scalaz.Leibniz._
import scalaz.Monoid

trait EvalDouble extends EvalArrow with DoubleRepr[Loss, Eval] {
  case class DLoss(d: Double)

  object DEC extends EvalCase[Double] {
    override type ret = Double
  }

  object DLC extends LossCase[Double] {
    override type ret = Unit
  }

  def dEval(d: Double) = new Eval[Double] {
    override val loss: Loss[Double] = DoubleInfo

    override def eval: Double = d

    override val ec: EvalCase.Aux[Double, Double] = DEC

    override def eca: ec.ret = d
  }

  def deval(d: Eval[Double]): Double = witness(d.ec.unique(DEC))(d.eca)

  override def LitD: Double => Eval[Double] = dEval

  override def PlusD: Eval[Double => Double => Double] =
    arrEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](l =>
      (arrEval[Double, Double, DLoss, DLoss](
        r => (dEval(deval(l) + deval(r)), rl => rl)),
        ll => DLoss(ll.seq.map(_._2.d).sum)))

  override def MultD: Eval[Double => Double => Double] =
    arrEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](l =>
      (arrEval[Double, Double, DLoss, DLoss](
        r => (dEval(deval(l) * deval(r)), rl => DLoss(deval(l) * rl.d))),
        ll => DLoss(ll.seq.map(l => deval(l._1) * l._2.d).sum)))

  override implicit def DoubleInfo: Loss.Aux[Double, DLoss] = new Loss[Double] {
    override def m: Monoid[DLoss] = new Monoid[DLoss] {
      override def zero: DLoss = DLoss(0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.d + f2.d)
    }

    override def conv: Double => Eval[Double] = dEval

    override val lc: LossCase.Aux[Double, DLC.ret] = DLC

    override def lca: lc.ret = ()

    override type ret = DLoss
  }
}
