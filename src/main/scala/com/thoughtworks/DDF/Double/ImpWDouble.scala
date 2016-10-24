package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Product.ProductRepr
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWDouble[Info[_], Repr[_]] extends
  Double[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {

  def base: Double[Info, Repr]

  def baseE: Double[Loss, BEval] = BEvalDouble.apply

  override implicit def doubleInfo: (Info[scala.Double], Loss[scala.Double]) = (base.doubleInfo, baseE.doubleInfo)

  override def litD = x => ImpW(base.litD(x), baseE.litD(x))(rcun, becun)

  override def plusD = ImpW(base.plusD, baseE.plusD)(rcun, becun)

  override def multD = ImpW(base.multD, baseE.multD)(rcun, becun)

  override def divD = ImpW(base.divD, baseE.divD)(rcun, becun)

  override def expD = ImpW(base.expD, baseE.expD)(rcun, becun)

  override def sigD = ImpW(base.sigD, baseE.sigD)(rcun, becun)
}

object ImpWDouble {
  implicit def apply[Info[_], Repr[_]](implicit
                                       d: Double[Info, Repr],
                                       p: ProductRepr[Info, Repr],
                                       c: Comb[Info, Repr],
                                       u: Unit[Info, Repr]) = new ImpWDouble[Info, Repr] {
    override def rp: ProductRepr[Info, Repr] = p

    override def rcomb: Comb[Info, Repr] = c

    override def runit: Unit[Info, Repr] = u

    override def base: Double[Info, Repr] = d
  }
}
