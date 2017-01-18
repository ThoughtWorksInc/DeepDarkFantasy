package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.Language.{LangInfoG, LangTermLang}

trait ADEvalProd extends Prod[ADEvalCase, ADEval] with ADEvalArr {
  override implicit val base = LangTermLang

  def pfem[A, B] = new ADEvalMatch[(A, B)] {
    override type ret = (ADEvalCase[A], ADEvalCase[B])
  }

  override implicit def prodInfo[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]):
  ADEvalCase.Aux[(A, B), Lambda[G => (ai.WithGrad[G], bi.WithGrad[G])]] =
    new ADEvalCase[(A, B)] {
      override type WithGrad[G] = (ai.WithGrad[G], bi.WithGrad[G])

      override def wgi[G: Gradient]: LangInfoG[(ai.WithGrad[G], bi.WithGrad[G])] = base.prodInfo(ai.wgi[G], bi.wgi[G])

      override val tm = pfem[A, B]

      override def tmr: tm.ret = (ai, bi)
    }

  override def prodZroInfo[A, B]: ADEvalCase[(A, B)] => ADEvalCase[A] = _.get(pfem[A, B])._1

  override def prodFstInfo[A, B]: ADEvalCase[(A, B)] => ADEvalCase[B] = _.get(pfem[A, B])._2

  override def mkProd[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[A => B => (A, B)] {
      override val fec = aInfo(ai, aInfo(bi, prodInfo(ai, bi)))

      override def term[G: Gradient] = base.mkProd(ai.wgi[G], bi.wgi[G])
    }

  override def uncurry[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[(A => B => C) => ((A, B)) => C] {
      override val fec = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(prodInfo(ai, bi), ci))

      override def term[G: Gradient] = base.uncurry(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def curry[A, B, C](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C]) =
    new ADEval[(((A, B)) => C) => A => B => C] {
      override val fec = aInfo(aInfo(prodInfo(ai, bi), ci), aInfo(ai, aInfo(bi, ci)))

      override def term[G: Gradient] = base.curry(ai.wgi[G], bi.wgi[G], ci.wgi[G])
    }

  override def fst[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[((A, B)) => B] {
      override val fec = aInfo(prodInfo(ai, bi), bi)

      override def term[G: Gradient] = base.fst(ai.wgi[G], bi.wgi[G])
    }

  override def zro[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]) =
    new ADEval[((A, B)) => A] {
      override val fec = aInfo(prodInfo(ai, bi), ai)

      override def term[G: Gradient] = base.zro(ai.wgi[G], bi.wgi[G])
    }

  override def ><[A, B, C, D](implicit ai: ADEvalCase[A], bi: ADEvalCase[B], ci: ADEvalCase[C], di: ADEvalCase[D]) =
    new ADEval[(A => C) => (B => D) => ((A, B)) => (C, D)] {
      override val fec = aInfo(aInfo(ai, ci), aInfo(aInfo(bi, di), aInfo(prodInfo(ai, bi), prodInfo(ci, di))))

      override def term[G: Gradient] = base.><(ai.wgi[G], bi.wgi[G], ci.wgi[G], di.wgi[G])
    }
}

object ADEvalProd extends ADEvalProd