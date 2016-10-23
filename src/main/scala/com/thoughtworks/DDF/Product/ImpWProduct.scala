package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWProduct[Info[_], Repr[_]] extends
  ProductMin[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {
  def base: ProductRepr[Info, Repr]

  def runit: Unit[Info, Repr]

  def baseE: ProductRepr[Loss, BEval] = BEvalProduct.apply
}

object ImpWProduct {
  implicit def apply[Info[_], Repr[_]] = new ImpWProduct[Info, Repr] {
    override def rp: ProductRepr[Info, Repr] = ???

    override def rcomb: Comb[Info, Repr] = ???

    override def base: ProductRepr[Info, Repr] = rp

    override def runit: Unit[Info, Repr] = ???

    override def mkProduct[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (A) => (B) => (A, B)] = ???

    override def zeroth[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, ((A, B)) => A] = ???

    override def first[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, ((A, B)) => B] = ???

    override implicit def productInfo[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): (Info[(A, B)], Loss[(A, B)]) = ???

    override def productZerothInfo[A, B]: ((Info[(A, B)], Loss[(A, B)])) => (Info[A], Loss[A]) = ???

    override def productFirstInfo[A, B]: ((Info[(A, B)], Loss[(A, B)])) => (Info[B], Loss[B]) = ???
  }
}
