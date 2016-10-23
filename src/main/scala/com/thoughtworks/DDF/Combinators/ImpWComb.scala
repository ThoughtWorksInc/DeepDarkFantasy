package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Product.ProductRepr
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWComb[Info[_], Repr[_]] extends
  Comb[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {
  def base: Comb[Info, Repr]

  def baseE: Comb[Loss, BEval] = BEvalComb.apply

  override def K[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])) =
    ImpW(base.K(ai._1, bi._1), baseE.K(ai._2, bi._2))(rcun, becun)

  override def Let[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])) =
    ImpW(base.Let(ai._1, bi._1), baseE.Let(ai._2, bi._2))(rcun, becun)

  override def App[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])) =
    ImpW(base.App(ai._1, bi._1), baseE.App(ai._2, bi._2))(rcun, becun)

  override def S[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])) =
    ImpW(base.S(ai._1, bi._1, ci._1), baseE.S(ai._2, bi._2, ci._2))(rcun, becun)

  override def Y[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])) =
    ImpW(base.Y(ai._1, bi._1), baseE.Y(ai._2, bi._2))(rcun, becun)

  override def C[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])) =
    ImpW(base.C(ai._1, bi._1, ci._1), baseE.C(ai._2, bi._2, ci._2))(rcun, becun)

  override def I[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, A => A] =
    ImpW(base.I(ai._1), baseE.I(ai._2))(rcun, becun)

  override def W[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])) =
    ImpW(base.W(ai._1, bi._1), baseE.W(ai._2, bi._2))(rcun, becun)

  override def B[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])) =
    ImpW(base.B(ai._1, bi._1, ci._1), baseE.B(ai._2, bi._2, ci._2))(rcun, becun)

  override def rcomb: Comb[Info, Repr] = base
}

object ImpWComb {
  implicit def apply[Info[_], Repr[_]](implicit
                                       c: Comb[Info, Repr],
                                       p: ProductRepr[Info, Repr],
                                       u: Unit[Info, Repr]): ImpWComb[Info, Repr] = new ImpWComb[Info, Repr] {
    override def rp: ProductRepr[Info, Repr] = p

    override def runit: Unit[Info, Repr] = u

    override def base: Comb[Info, Repr] = c
  }
}
