package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}

trait GArrDouble extends Gradient[Double => Double] {
  override implicit def GInfo: LangInfoG[Double => Double] = ltl.aInfo(ltl.doubleInfo, ltl.doubleInfo)

  override def constG: LangTerm[Double => Double] = ltl.I(ltl.doubleInfo)

  def lift: LangTerm[Double => Double] =>
    LangTerm[((Double, Double => Double)) => Double => Double] =>
      LangTerm[((Double, Double => Double)) => (Double, Double => Double)] = origf => gradf => {
    ltl.S__(ltl.B__(ltl.mkProd[Double, Double => Double])(ltl.B__(origf)(ltl.zro[Double, Double => Double])))(gradf)
  }

  def lift2: LangTerm[Double => Double => Double] =>
    LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => Double => Double] =>
      LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] =
    origf => gradf => {
      ltl.curry_(ltl.S__(
        ltl.B__(ltl.mkProd[Double, Double => Double])(
          ltl.uncurry_(ltl.C_(ltl.B__(
            ltl.C_(ltl.B__(origf)(ltl.zro[Double, Double => Double])))(ltl.zro[Double, Double => Double])))))(
        ltl.uncurry_(gradf)))
    }

  override def plus:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] =
    lift2(ltl.plusD)(
      ltl.B__(
        ltl.C_(ltl.B__({
          implicit val l = NextLang.apply[LangInfoG, LangTerm, Double => Double]
          val r = NextLang.apply[LangInfoG, l.repr, Double => Double]
          l.collapse(r.collapse(r.S__[Double, Double, Double](r.B__(r.plusD)(r.rconv(l.in)))(r.in)))
        })(ltl.fst[Double, Double => Double])))(
        ltl.fst[Double, Double => Double]))

  override def mult:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] =
    lift2(ltl.multD)(???)

  override def div:
  LangTerm[((Double, Double => Double)) => ((Double, Double => Double)) => (Double, Double => Double)] =
    lift2(ltl.divD)(???)

  override def sig: LangTerm[((Double, Double => Double)) => (Double, Double => Double)] =
    lift(ltl.sigD)(???)

  override def exp: LangTerm[((Double, Double => Double)) => (Double, Double => Double)] =
    lift(ltl.expD)(???)
}

object GArrDouble extends GArrDouble