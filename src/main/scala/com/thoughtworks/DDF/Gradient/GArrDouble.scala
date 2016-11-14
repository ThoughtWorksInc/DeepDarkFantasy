package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GArrDouble extends Gradient[Double => Double] {
  override implicit def GInfo: LangInfoG[Double => Double] = ltl.aInfo(ltl.doubleInfo, ltl.doubleInfo)

  override def constG: LangTerm[Double => Double] = ltl.I(ltl.doubleInfo)

  override def plus:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def mult:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def div:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def sig: LangTerm[((Double, Double => Double)) => (Double, Double => Double)] = ???

  override def exp: LangTerm[((Double, Double => Double)) => (Double, Double => Double)] = ???
}

object GArrDouble extends GArrDouble