package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.InfoBase.ImpWInfoBase
import com.thoughtworks.DDF.Product.{BEvalProduct, Product}
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWArrowMin[Info[_], Repr[_]] extends
  ArrowMin[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with ImpWInfoBase[Info, Repr] {
  def bep: Product[Loss, BEval] = BEvalProduct.apply

  def rp: Product[Info, Repr]

  override def app[A, B]: ImpW[Info, Repr, A => B] => ImpW[Info, Repr, A] => ImpW[Info, Repr, B] = f => x =>
    new ImpW[Info, Repr, B] {
      override type Weight = (f.Weight, x.Weight)
      override implicit val l: Loss[(f.Weight, x.Weight)] = bep.productInfo(f.l, x.l)
      override val w: (f.Weight, x.Weight) = (f.w, x.w)
      override val exp: Repr[((f.Weight, x.Weight)) => B] =
        rcomb.S__(rcomb.B__(f.exp)(rp.zeroth(f.wi, x.wi)))(rcomb.B__(x.exp)(rp.first(f.wi, x.wi)))
      override def eval: BEval[((f.Weight, x.Weight)) => B] =
        becomb.S__(becomb.B__(f.eval)(bep.zeroth(f.l, x.l)))(becomb.B__(x.eval)(bep.first(f.l, x.l)))
      override def wi: Info[(f.Weight, x.Weight)] = rp.productInfo(f.wi, x.wi)
    }

  override implicit def arrowInfo[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])):
  (Info[A => B], Loss[A => B]) = (rp.arrowInfo(ai._1, bi._1), bep.arrowInfo(ai._2, bi._2))

  override def arrowDomainInfo[A, B]: ((Info[A => B], Loss[A => B])) => (Info[A], Loss[A]) = x =>
    (rp.arrowDomainInfo(x._1), bep.arrowDomainInfo(x._2))

  override def arrowRangeInfo[A, B]: ((Info[A => B], Loss[A => B])) => (Info[B], Loss[B]) = x =>
    (rp.arrowRangeInfo(x._1), bep.arrowRangeInfo(x._2))
}

object ImpWArrowMin {
  implicit def apply[Info[_], Repr[_]](implicit
                                       rprod: Product[Info, Repr],
                                       rc: Comb[Info, Repr],
                                       u: Unit[Info, Repr]):
  ImpWArrowMin[Info, Repr] = new ImpWArrowMin[Info, Repr] {
    override def rp: Product[Info, Repr] = rprod

    override def rcomb: Comb[Info, Repr] = rc

    override def runit: Unit[Info, Repr] = u
  }
}
