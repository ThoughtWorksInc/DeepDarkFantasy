package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch, Gradient}
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}

trait FEvalProd extends Prod[FEvalCase, FEval] with FEvalArr {
  override val base = LangTermLang

  def pfem[A, B] = new FEvalMatch[(A, B)] {
    override type ret = (FEvalCase[A], FEvalCase[B])
  }

  override implicit def prodInfo[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]):
  FEvalCase.Aux[(A, B), Lambda[G => (ai.WithGrad[G], bi.WithGrad[G])]] =
    new FEvalCase[(A, B)] {
      override type WithGrad[G] = (ai.WithGrad[G], bi.WithGrad[G])

      override def wgi[G: Gradient]: LangInfoG[(ai.WithGrad[G], bi.WithGrad[G])] = base.prodInfo(ai.wgi[G], bi.wgi[G])

      override val tm = pfem[A, B]

      override def tmr: tm.ret = (ai, bi)
    }

  override def prodZroInfo[A, B]: FEvalCase[(A, B)] => FEvalCase[A] = _.get(pfem[A, B])._1

  override def prodFstInfo[A, B]: FEvalCase[(A, B)] => FEvalCase[B] = _.get(pfem[A, B])._2

  override def mkProd[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[A => B => (A, B)] {
      override val fec = aInfo(ai, aInfo(bi, prodInfo(ai, bi)))

      override def term[G: Gradient] = base.mkProd(ai.wgi[G], bi.wgi[G])
    }

  override def uncurry[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[(A => B => C) => ((A, B)) => C] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(prodInfo(ai, bi), ci))

      override def term[G: Gradient] = base.uncurry(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def curry[A, B, C](implicit ai: FEvalCase[A], bi: FEvalCase[B], ci: FEvalCase[C]) =
    new FEval[(((A, B)) => C) => A => B => C] {
      override val fec = aInfo(aInfo(prodInfo(ai, bi), ci), aInfo(ai, aInfo(bi, ci)))

      override def term[G: Gradient] = base.curry(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def fst[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[((A, B)) => B] {
      override val fec = aInfo(prodInfo(ai, bi), bi)

      override def term[G: Gradient] = base.fst(ai.wgi[G], bi.wgi[G])
    }

  override def zro[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]) =
    new FEval[((A, B)) => A] {
      override val fec = aInfo(prodInfo(ai, bi), ai)

      override def term[G: Gradient] = base.zro(ai.wgi[G], bi.wgi[G])
    }
}

object FEvalProd extends FEvalProd