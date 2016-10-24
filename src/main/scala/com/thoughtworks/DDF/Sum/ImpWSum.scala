package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ImpWArrowMin
import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Product.Product
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWSum[Info[_], Repr[_]] extends
  Sum[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWArrowMin[Info, Repr] {
  def base: Sum[Info, Repr]

  def baseE: Sum[Loss, BEval] = BEvalSum.apply

  override def sumComm[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, Either[A, B] => Either[B, A]] =
    ImpW(base.sumComm(ai._1, bi._1), baseE.sumComm(ai._2, bi._2))(rcun, becun)

  override def sumAssocLR[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])):
  ImpW[Info, Repr, Either[Either[A, B], C] => Either[A, Either[B, C]]] =
    ImpW(base.sumAssocLR(ai._1, bi._1, ci._1), baseE.sumAssocLR(ai._2, bi._2, ci._2))(rcun, becun)

  override def sumAssocRL[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])):
  ImpW[Info, Repr, Either[A, Either[B, C]] => Either[Either[A, B], C]] =
    ImpW(base.sumAssocRL(ai._1, bi._1, ci._1), baseE.sumAssocRL(ai._2, bi._2, ci._2))(rcun, becun)

  override def left[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, A => Either[A, B]] =
    ImpW(base.left(ai._1, bi._1), baseE.left(ai._2, bi._2))(rcun, becun)

  override def right[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  ImpW[Info, Repr, B => Either[A, B]] = ImpW(base.right(ai._1, bi._1), baseE.right(ai._2, bi._2))(rcun, becun)

  override def sumMatch[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])):
  ImpW[Info, Repr, (Either[A, B]) => ((A) => C) => ((B) => C) => C] =
    ImpW(base.sumMatch(ai._1, bi._1, ci._1), baseE.sumMatch(ai._2, bi._2, ci._2))(rcun, becun)

  override implicit def sumInfo[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  (Info[Either[A, B]], Loss[Either[A, B]]) = (base.sumInfo(ai._1, bi._1), baseE.sumInfo(ai._2, bi._2))

  override def sumLeftInfo[A, B]: ((Info[Either[A, B]], Loss[Either[A, B]])) => (Info[A], Loss[A]) = i =>
    (base.sumLeftInfo(i._1), baseE.sumLeftInfo(i._2))

  override def sumRightInfo[A, B]: ((Info[Either[A, B]], Loss[Either[A, B]])) => (Info[B], Loss[B]) = i =>
    (base.sumRightInfo(i._1), baseE.sumRightInfo(i._2))
}

object ImpWSum {
  implicit def apply[Info[_], Repr[_]](implicit
                                       p: Product[Info, Repr],
                                       c: Comb[Info, Repr],
                                       s: Sum[Info, Repr],
                                       u: Unit[Info, Repr]): ImpWSum[Info, Repr] = new ImpWSum[Info, Repr] {
    override def rp: Product[Info, Repr] = p

    override def rcomb: Comb[Info, Repr] = c

    override def base: Sum[Info, Repr] = s

    override def runit: Unit[Info, Repr] = u
  }
}