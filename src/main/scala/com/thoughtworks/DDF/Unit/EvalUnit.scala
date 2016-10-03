package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase, MonoidUnit}

import scalaz.Monoid

trait EvalUnit extends UnitLang[Loss, Eval] {
  override implicit def UnitInfo: Loss.Aux[Unit, Unit] = new Loss[Unit] {
    override type ret = Unit

    override def m: Monoid[Unit] = MonoidUnit.apply

    override def convert: Unit => Eval[Unit] = _ => mkUnit

    override val lc: LossCase.Aux[Unit, Unit] = new LossCase[Unit] {
      override type ret = Unit
    }

    override def lca: lc.ret = ()
  }

  override def mkUnit: Eval[Unit] = new Eval[Unit] {
    override def eca: ec.ret = ()

    override def eval: Unit = ()

    override val ec: EvalCase.Aux[Unit, Unit] = new EvalCase[Unit] {
      override type ret = Unit
    }

    override implicit val loss: Loss[Unit] = UnitInfo
  }

}
