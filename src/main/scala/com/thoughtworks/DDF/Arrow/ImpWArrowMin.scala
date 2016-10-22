package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Combinators.BEvalComb
import com.thoughtworks.DDF.Product.{BEvalProduct, ProductRepr}
import com.thoughtworks.DDF.{BEval, ImpW, Loss, NoInfo}

trait ImpWArrowMin[Info[_], Repr[_]] extends ArrowMin[NoInfo, ImpW[Info, Repr, ?]] with SimpleArrow[ImpW[Info, Repr, ?]] {
  def bep: ProductRepr[Loss, BEval] = BEvalProduct.apply
  def rp: ProductRepr[Info, Repr]
  def becomb: Comb[Loss, BEval] = BEvalComb.apply
  def rcomb: Comb[Info, Repr]
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
}

object ImpWArrowMin {
  implicit def apply[Info[_], Repr[_]](implicit rprod: ProductRepr[Info, Repr], rc: Comb[Info, Repr]):
  ImpWArrowMin[Info, Repr] = new ImpWArrowMin[Info, Repr] {
    override def rp: ProductRepr[Info, Repr] = rprod

    override def rcomb: Comb[Info, Repr] = rc
  }
}
