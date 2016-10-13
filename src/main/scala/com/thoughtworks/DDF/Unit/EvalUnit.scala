package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{CommutativeMonoid, CommutativeMonoidUnit, Eval, EvalCase, Loss, LossCase}

trait EvalUnit extends UnitRepr[Loss, Eval] {
  override implicit def unitInfo: Loss.Aux[Unit, Unit] = new Loss[Unit] {
    override type ret = Unit

    override def m: CommutativeMonoid[Unit] = CommutativeMonoidUnit.apply

    override def convert: Unit => Eval[Unit] = _ => mkUnit

    override val lc: LossCase.Aux[Unit, Unit] = new LossCase[Unit] {
      override type ret = Unit
    }

    override def lca: lc.ret = ()

    override def update(x: Unit)(rate: Double)(l: loss): Unit = ()
  }

  override def mkUnit: Eval[Unit] = new Eval[Unit] {
    override def eca: ec.ret = ()

    override def eval: Unit = ()

    override val ec: EvalCase.Aux[Unit, Unit] = new EvalCase[Unit] {
      override type ret = Unit
    }

    override implicit val loss: Loss[Unit] = unitInfo
  }

}
