package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.EvalOArr
import com.thoughtworks.DDF.Language.{InterLangInfoG, InterLangTerm}
import com.thoughtworks.DDF.{EvalO, EvalOMatch}

trait EvalOProdMin extends
  ProdMin[InterLangInfoG, EvalO] with
  ILIGProdType[EvalO] with
  EvalOArr {
  def ptm[A, B]: EvalOMatch.Aux[(A, B), (EvalO[A], EvalO[B])] = new EvalOMatch[(A, B)] {
    override type ret = (EvalO[A], EvalO[B])
  }

  override def mkProd[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => B => (A, B)] =
    aeval(ltl.mkProd[A, B])(x => aeval(ltl.mkProd_[A, B](x.l))(y => new EvalO[(A, B)] {
      override def l: InterLangTerm[(A, B)] = ltl.mkProd__(x.l)(y.l)

      override def tmr: tm.ret = (x, y)

      override val tm = ptm[A, B]
    }))

  override def zro[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[((A, B)) => A] =
    aeval(ltl.zro[A, B])(_.get(ptm[A, B])._1)

  override def fst[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[((A, B)) => B] =
    aeval(ltl.fst[A, B])(_.get(ptm[A, B])._2)
}

object EvalOProdMin extends EvalOProdMin