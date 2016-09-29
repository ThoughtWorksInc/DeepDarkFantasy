package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.{Eval, EvalCase, Loss, LossCase}

import scalaz.Monoid

trait UnitEval extends UnitLang[Loss, Eval] {
  override implicit def UnitInfo: Loss.Aux[Unit, Unit] = new Loss[Unit] {
    override type ret = Unit

    override def m: Monoid[loss] = new Monoid[loss] {
      override def zero: loss = ()

      override def append(f1: loss, f2: => loss): loss = ()
    }

    override def conv: Unit => Eval[Unit] = _ => mkUnit

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

    override val loss: Loss[Unit] = UnitInfo
  }

}
