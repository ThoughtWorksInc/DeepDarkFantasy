package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{CommutativeMonoid, CommutativeMonoidUnit, BEval, BEvalCase, Loss, LossCase}

trait BEvalUnit extends UnitRepr[Loss, BEval] {
  override implicit def unitInfo: Loss.Aux[Unit, Unit] = new Loss[Unit] {
    override type ret = Unit

    override def m: CommutativeMonoid[Unit] = CommutativeMonoidUnit.apply

    override def convert: Unit => BEval[Unit] = _ => mkUnit

    override val lc: LossCase.Aux[Unit, Unit] = new LossCase[Unit] {
      override type ret = Unit
    }

    override def lca: lc.ret = ()

    override def update(x: Unit)(rate: Double)(l: loss): Unit = ()
  }

  override def mkUnit: BEval[Unit] = new BEval[Unit] {
    override def eca: ec.ret = ()

    override def eval: Unit = ()

    override val ec: BEvalCase.Aux[Unit, Unit] = new BEvalCase[Unit] {
      override type ret = Unit
    }

    override implicit val loss: Loss[Unit] = unitInfo
  }

}
