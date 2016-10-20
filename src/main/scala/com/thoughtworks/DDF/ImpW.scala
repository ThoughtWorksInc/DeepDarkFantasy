package com.thoughtworks.DDF

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.CombUnit.CombUnitL

trait ImpW[Repr[_], T] {
  ext =>
  type Weight

  implicit val l: Loss[Weight]

  val w: Weight

  val exp: Repr[Weight => T]

  def eval: BEval[Weight => T]

  def update[TL](rate: Double, tl: TL)(implicit ti: Loss.Aux[T, TL]): ImpW[Repr, T] = {
    val newW = l.update(w)(rate)(new BEvalArrow {}.aeval(eval).forward(ext.l.convert(w)).backward(tl))
    new ImpW[Repr, T] {
      override type Weight = ext.Weight

      override implicit val l: Loss[ext.Weight] = ext.l

      override val w: ext.Weight = newW

      override val exp: Repr[ext.Weight => T] = ext.exp

      override def eval: BEval[ext.Weight => T] = ext.eval
    }
  }
}

object ImpW {
  def apply[Repr[_], W, T](lw: Loss[W], weight: W, expwt: Repr[W => T], evalwt: BEval[W => T]) = new ImpW[Repr, T] {
    override type Weight = W

    override implicit val l: Loss[W] = lw

    override val w: W = weight

    override val exp: Repr[W => T] = expwt

    override def eval: BEval[W => T] = evalwt
  }

  def unitW[Info[_], Repr[_], T](expT: Repr[T], evalT: BEval[T])(
    implicit cuex: CombUnitL[Info, Repr], cuev: CombUnitL[Loss, BEval]) =
    new ImpW[Repr, T] {
      override type Weight = Unit

      override implicit val l: Loss[Unit] = cuev.unitInfo

      override val w: Unit = ()

      override val exp: Repr[Unit => T] = cuex.app(cuex.K(cuex.reprInfo(expT), cuex.unitInfo))(expT)

      override def eval: BEval[Unit => T] = cuev.app(cuev.K(cuev.reprInfo(evalT), cuev.unitInfo))(evalT)
    }
}