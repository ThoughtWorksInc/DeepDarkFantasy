package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{CommutativeMonoid, CommutativeMonoidUnit, BEval, BEvalCase, Loss, LossCase}

trait BEvalUnit extends Unit[Loss, BEval] {
  override implicit def unitInfo: Loss.Aux[scala.Unit, scala.Unit] = new Loss[scala.Unit] {
    override type ret = scala.Unit

    override def m: CommutativeMonoid[scala.Unit] = CommutativeMonoidUnit.apply

    override def convert: scala.Unit => BEval[scala.Unit] = _ => mkUnit

    override val lc: LossCase.Aux[scala.Unit, scala.Unit] = new LossCase[scala.Unit] {
      override type ret = scala.Unit
    }

    override def lca: lc.ret = ()

    override def update(x: scala.Unit)(rate: Double)(l: loss): scala.Unit = ()
  }

  override def mkUnit: BEval[scala.Unit] = new BEval[scala.Unit] {
    override def eca: ec.ret = ()

    override def eval: scala.Unit = ()

    override val ec: BEvalCase.Aux[scala.Unit, scala.Unit] = new BEvalCase[scala.Unit] {
      override type ret = scala.Unit
    }

    override implicit val loss: Loss[scala.Unit] = unitInfo
  }

}
