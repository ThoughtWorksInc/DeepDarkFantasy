package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Product.Product
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWOption[Info[_], Repr[_]] extends
  Option[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {
  def base: Option[Info, Repr]

  def baseE: Option[Loss, BEval] = BEvalOption.apply

  override def none[A](implicit ai: (Info[A], Loss[A])) = ImpW(base.none(ai._1), baseE.none(ai._2))(rcun, becun)

  override def some[A](implicit ai: (Info[A], Loss[A])) = ImpW(base.some(ai._1), baseE.some(ai._2))(rcun, becun)

  override def optionMatch[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])) =
    ImpW(base.optionMatch(ai._1, bi._1), baseE.optionMatch(ai._2, bi._2))(rcun, becun)

  override implicit def optionInfo[A](implicit ai: (Info[A], Loss[A])) =
    (base.optionInfo(ai._1), baseE.optionInfo(ai._2))

  override def optionElmInfo[A] = i => (base.optionElmInfo(i._1), baseE.optionElmInfo(i._2))
}

object ImpWOption {
  implicit def apply[Info[_], Repr[_]](implicit
                                       o: Option[Info, Repr],
                                       p: Product[Info, Repr],
                                       c: Comb[Info, Repr],
                                       u: Unit[Info, Repr]) = new ImpWOption[Info, Repr] {

    override def rp: Product[Info, Repr] = p

    override def rcomb: Comb[Info, Repr] = c

    override def runit: Unit[Info, Repr] = u

    override def base: Option[Info, Repr] = o
  }
}
