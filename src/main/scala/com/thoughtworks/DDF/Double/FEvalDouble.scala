package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Bool.FEvalBool
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.Product.FEvalProd
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase, Gradient}

trait FEvalDouble[G] extends
  Double[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalArr[G] with
  FEvalBool[G] with
  FEvalProd[G] {
  val grad: Gradient[G]

  override def ltD =
    new FEval[G, scala.Double => scala.Double => Boolean] {
      override val tm = aInfo(doubleInfo, aInfo(doubleInfo, boolInfo))

      override val deriv =
        base.C_(base.B__(
          base.C_(base.B__(base.ltD)(base.zro(base.doubleInfo, grad.GInfo)))
        )(base.zro(base.doubleInfo, grad.GInfo)))
    }

  override def divD = new FEval[G, scala.Double => scala.Double => scala.Double] {
    override val tm = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override val deriv = grad.div
  }

  override def multD = new FEval[G, scala.Double => scala.Double => scala.Double] {
    override val tm = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override val deriv = grad.mult
  }

  override def expD = new FEval[G, scala.Double => scala.Double] {
    override val tm = aInfo(doubleInfo, doubleInfo)

    override val deriv = grad.exp
  }

  override def sigD = new FEval[G, scala.Double => scala.Double] {
    override val tm = aInfo(doubleInfo, doubleInfo)

    override val deriv = grad.sig
  }

  override def plusD = new FEval[G, scala.Double => scala.Double => scala.Double] {
    override val tm = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override val deriv = grad.plus
  }

  override def litD: scala.Double => FEval[G, scala.Double] = d =>
    new FEval[G, scala.Double] {
      override val tm = doubleInfo

      override val deriv = base.mkProd__(base.litD(d))(grad.constG)
    }

  override implicit def doubleInfo: FEvalCase.Aux[G, scala.Double, (scala.Double, G)] =
    new FEvalCase[G, scala.Double] {
      override type ret = (scala.Double, G)

      override val tm: FEMMatch.Aux[G, scala.Double, Unit] = new FEMMatch[G, scala.Double] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()

      override def lr: LangInfoG[(scala.Double, G)] = base.prodInfo(base.doubleInfo, grad.GInfo)
    }
}

object FEvalDouble {
  implicit def apply[G](implicit g: Gradient[G]): FEvalDouble[G] = new FEvalDouble[G] {
    override val grad: Gradient[G] = g
  }
}