package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.{Arr, FEvalArr}
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalProd[G] extends Prod[FEvalCase[G, ?], FEval[G, ?]] with FEvalArr[G] {
  override val base = LangTermLang

  def pfem[A, B] = new FEMMatch[G, (A, B)] {
    override type ret = (FEvalCase[G, A], FEvalCase[G, B])
  }

  override implicit def prodInfo[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]):
  FEvalCase.Aux[G, (A, B), (ai.ret, bi.ret)] =
    new FEvalCase[G, (A, B)] {
      override type ret = (ai.ret, bi.ret)

      override def lr: LangInfoG[(ai.ret, bi.ret)] = base.prodInfo(ai.lr, bi.lr)

      override val tm = pfem[A, B]

      override def tmr: tm.ret = (ai, bi)
    }

  override def prodZroInfo[A, B]: FEvalCase[G, (A, B)] => FEvalCase[G, A] = _.get(pfem[A, B])._1

  override def prodFstInfo[A, B]: FEvalCase[G, (A, B)] => FEvalCase[G, B] = _.get(pfem[A, B])._2

  override def mkProduct[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, A => B => (A, B)] {
      override val tm = aInfo(ai, aInfo(bi, prodInfo(ai, bi)))

      override val deriv = base.mkProduct(ai.lr, bi.lr)
    }

  override def uncurry[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (A => B => C) => ((A, B)) => C] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(prodInfo(ai, bi), ci))

      override val deriv = base.uncurry(ai.lr, bi.lr, ci.lr)
    }

  override def curry[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (((A, B)) => C) => A => B => C] {
      override val tm = aInfo(aInfo(prodInfo(ai, bi), ci), aInfo(ai, aInfo(bi, ci)))

      override val deriv = base.curry(ai.lr, bi.lr, ci.lr)
    }

  override def fst[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, ((A, B)) => B] {
      override val tm = aInfo(prodInfo(ai, bi), bi)

      override val deriv = base.fst(ai.lr, bi.lr)
    }

  override def zro[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, ((A, B)) => A] {
      override val tm = aInfo(prodInfo(ai, bi), ai)

      override val deriv = base.zro(ai.lr, bi.lr)
    }
}

object FEvalProd {
  implicit def apply[G]: FEvalProd[G] = new FEvalProd[G] { }
}