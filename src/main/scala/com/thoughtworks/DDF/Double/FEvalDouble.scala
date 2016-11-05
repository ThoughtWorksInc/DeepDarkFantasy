package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Bool.FEvalBool
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalDouble[G] extends
  Double[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalArr[G] with
  FEvalBool[G] {
  override def ltD: FEval[G, scala.Double => scala.Double => Boolean] = ???

  override def divD: FEval[G, scala.Double => scala.Double => scala.Double] = ???

  override def multD: FEval[G, scala.Double => scala.Double => scala.Double] = ???

  override def expD: FEval[G, scala.Double => scala.Double] = ???

  override def sigD: FEval[G, scala.Double => scala.Double] = ???

  override def plusD: FEval[G, scala.Double => scala.Double => scala.Double] = ???

  override def litD: scala.Double => FEval[G, scala.Double] = ???

  override implicit def doubleInfo: FEvalCase.Aux[G, scala.Double, (scala.Double, G)] =
    new FEvalCase[G, scala.Double] {
      override type ret = (scala.Double, G)

      override val tm: FEMMatch.Aux[G, scala.Double, Unit] = new FEMMatch[G, scala.Double] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()

      override def lr: LangInfoG[(scala.Double, G)] = ???
    }
}

object FEvalDouble {
  implicit def apply[G]: FEvalDouble[G] = new FEvalDouble[G] { }
}