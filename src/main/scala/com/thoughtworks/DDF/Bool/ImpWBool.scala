package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Product.Product
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWBool[Info[_], Repr[_]] extends
  Bool[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {
  def base: Bool[Info, Repr]

  def baseE: Bool[Loss, BEval] = BEvalBool.apply

  override implicit def boolInfo = (base.boolInfo, baseE.boolInfo)

  override def litB = b => ImpW(base.litB(b), baseE.litB(b))(rcun, becun)

  override def ite[A](implicit ai: (Info[A], Loss[A])) = ImpW(base.ite(ai._1), baseE.ite(ai._2))(rcun, becun)
}

object ImpWBool {
  implicit def apply[Info[_], Repr[_]](implicit
                                       b: Bool[Info, Repr],
                                       p: Product[Info, Repr],
                                       c: Comb[Info, Repr],
                                       u: Unit[Info, Repr]): ImpWBool[Info, Repr] = new ImpWBool[Info, Repr] {
    override def rp: Product[Info, Repr] = p

    override def rcomb: Comb[Info, Repr] = c

    override def runit: Unit[Info, Repr] = u

    override def base: Bool[Info, Repr] = b
  }
}