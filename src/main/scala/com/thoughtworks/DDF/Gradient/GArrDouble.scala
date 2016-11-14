package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GArrDouble extends Gradient[Double => Double] {
  override implicit def GInfo: LangInfoG[Double => Double] = ltl.aInfo(ltl.doubleInfo, ltl.doubleInfo)

  override def constG: LangTerm[Double => Double] = ltl.I(ltl.doubleInfo)

  def lift: LangTerm[Double => Double] =>
    LangTerm[((Double, Double => Double)) => Double => Double] =>
      LangTerm[((Double, Double => Double)) => (Double, Double => Double)] = origf => gradf => {
    ltl.S__(ltl.B__(ltl.mkProd[Double, Double => Double])(ltl.B__(origf)(ltl.zro[Double, Double => Double])))(gradf)
  }

  override def plus:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def mult:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def div:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def sig: LangTerm[((Double, Double => Double)) => (Double, Double => Double)] = lift(ltl.sigD)(???)

  override def exp: LangTerm[((Double, Double => Double)) => (Double, Double => Double)] = lift(ltl.expD)(???)
}

object GArrDouble extends GArrDouble