package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Bool.FEvalBool
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalDouble extends
  Double[FEvalCase, FEval] with
  FEvalArr with
  FEvalBool with
  FEvalProd {
  override def ltD =
    new FEval[scala.Double => scala.Double => Boolean] {
      override val fec = aInfo(doubleInfo, aInfo(doubleInfo, boolInfo))

      override def term[G: Gradient] =
        base.C_(base.B__(
          base.C_(base.B__(base.ltD)(base.zro(base.doubleInfo, implicitly[Gradient[G]].GInfo)))
        )(base.zro(base.doubleInfo, implicitly[Gradient[G]].GInfo)))
    }

  override def divD = new FEval[scala.Double => scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override def term[G: Gradient] = implicitly[Gradient[G]].div
  }

  override def multD = new FEval[scala.Double => scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override def term[G: Gradient] = implicitly[Gradient[G]].mult
  }

  override def expD = new FEval[scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, doubleInfo)

    override def term[G: Gradient] = implicitly[Gradient[G]].exp
  }

  override def sigD = new FEval[scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, doubleInfo)

    override def term[G: Gradient] = implicitly[Gradient[G]].sig
  }

  override def plusD = new FEval[scala.Double => scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override def term[G: Gradient] = implicitly[Gradient[G]].plus
  }

  override def litD: scala.Double => FEval[scala.Double] = d =>
    new FEval[scala.Double] {
      override val fec = doubleInfo

      override def term[G: Gradient] = base.mkProd__(base.litD(d))(implicitly[Gradient[G]].constG)
    }

  override implicit def doubleInfo: FEvalCase.Aux[scala.Double, Lambda[X => (scala.Double, X)]] =
    new FEvalCase[scala.Double] {
      override type WithGrad[G] = (scala.Double, G)

      override val tm = new FEvalMatch[scala.Double] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()

      override def wgi[G: Gradient]: LangInfoG[(scala.Double, G)] =
        base.prodInfo(base.doubleInfo, implicitly[Gradient[G]].GInfo)
    }
}

object FEvalDouble extends FEvalDouble