package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Unit.UnitRepr
import com.thoughtworks.DDF.{ImpW, NoInfo}

trait ImpWProduct[Info[_], Repr[_]] extends
  ProductMin[NoInfo, ImpW[Info, Repr, ?]] with
  SimpleProduct[ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {
  def base: ProductRepr[Info, Repr]

  def runit: UnitRepr[Info, Repr]

  override def mkProduct[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ImpW[Info, Repr, A => B => (A, B)] =
    ImpW.apply(base.mkProduct[A, B], ???)(runit)

  override def zeroth[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ImpW[Info, Repr, ((A, B)) => A] = ???

  override def first[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]): ImpW[Info, Repr, ((A, B)) => B] = ???
}

object ImpWProduct {
  implicit def apply[Info[_], Repr[_]] = new ImpWProduct[Info, Repr] {
    override def rp: ProductRepr[Info, Repr] = ???

    override def rcomb: Comb[Info, Repr] = ???
  }
}
