package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.Combinators.{BEvalComb, Comb}
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, CommutativeMonoidUnit, Loss, LossCase}

import scalaz.Leibniz._

trait BEvalBool extends Bool[Loss, BEval] with BEvalArrow {
  object BoolBEC extends BEvalCase[Boolean] {
    override type ret = Boolean
  }

  override def litB: Boolean => BEval[Boolean] = b => new BEval[Boolean] {
    override def eca: ec.ret = b

    override def eval: Boolean = b

    override val ec: BEvalCase.Aux[Boolean, Boolean] = BoolBEC

    override val loss: Loss[Boolean] = BoolInfo
  }

  def beval: BEval[Boolean] => Boolean = x => witness(x.ec.unique(BoolBEC))(x.eca)

  private def comb: Comb[Loss, BEval] = BEvalComb.apply

  override def ite[A](implicit ai: Loss[A]): BEval[Boolean => A => A => A] =
    arrowEval[Boolean, A => A => A, Unit, ArrowLoss[A, ArrowLoss[A, ai.loss]]](b =>
      (if(beval(b))
        comb.K[A, A](ai, ai) else
        app(comb.C[A, A, A](ai, ai, ai))(comb.K[A, A](ai, ai)), _ => ()))(BoolInfo, arrowInfo(ai, arrowInfo(ai, ai)))

  override implicit def BoolInfo: Loss.Aux[Boolean, Unit] = new Loss[Boolean] {
    override def convert = litB

    override def m: CommutativeMonoid[Unit] = CommutativeMonoidUnit.apply

    override def lca: lc.ret = ()

    override val lc: LossCase.Aux[Boolean, Unit] = new LossCase[Boolean] {
      override type ret = Unit
    }

    override type ret = Unit

    override def update(x: Boolean)(rate: Double)(l: loss): Boolean = x
  }
}

object BEvalBool {
  implicit def apply = new BEvalBool {}
}
