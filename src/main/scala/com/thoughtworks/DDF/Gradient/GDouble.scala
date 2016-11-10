package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GDouble extends Gradient[Double] {
  override implicit def GInfo: LangInfoG[Double] = ltl.doubleInfo

  override def constG: LangTerm[Double] = ltl.litD(0)

  override def plus: LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] = ???

  override def mult: LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] = ???

  override def div: LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] = ???

  override def sig: LangTerm[((Double, Double)) => (Double, Double)] = ???

  override def exp: LangTerm[((Double, Double)) => (Double, Double)] = ???
}

object GDouble extends GDouble