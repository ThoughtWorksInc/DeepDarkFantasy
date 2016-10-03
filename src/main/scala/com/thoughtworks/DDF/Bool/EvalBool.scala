package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.{ArrowLoss, EvalArrow}
import com.thoughtworks.DDF.Combinators.{Comb, EvalComb}
import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase, MonoidUnit}

import scalaz.Monoid
import scalaz.Leibniz._

trait EvalBool extends BoolRepr[Loss, Eval] with EvalArrow {
  object BoolEC extends EvalCase[Boolean] {
    override type ret = Boolean
  }

  override def litB: Boolean => Eval[Boolean] = b => new Eval[Boolean] {
    override def eca: ec.ret = b

    override def eval: Boolean = b

    override val ec: EvalCase.Aux[Boolean, Boolean] = BoolEC

    override val loss: Loss[Boolean] = BoolInfo
  }

  def beval: Eval[Boolean] => Boolean = x => witness(x.ec.unique(BoolEC))(x.eca)

  val comb: Comb[Loss, Eval] = EvalComb.apply

  import comb._

  override def ite[A](implicit ai: Loss[A]): Eval[Boolean => A => A => A] =
    arrowEval[Boolean, A => A => A, Unit, ArrowLoss[A, ArrowLoss[A, ai.loss]]](b =>
      (if(beval(b))
        K[A, A](ai, ai) else
        app(C[A, A, A](ai, ai, ai))(K[A, A](ai, ai)), _ => ()))(BoolInfo, ArrowInfo(ai, ArrowInfo(ai, ai)))

  override implicit def BoolInfo: Loss.Aux[Boolean, Unit] = new Loss[Boolean] {
    override def convert = litB

    override def m: Monoid[Unit] = MonoidUnit.apply

    override def lca: lc.ret = ()

    override val lc: LossCase.Aux[Boolean, Unit] = new LossCase[Boolean] {
      override type ret = Unit
    }

    override type ret = Unit
  }
}

object EvalBool {
  implicit def apply = new EvalBool {}
}
