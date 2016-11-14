package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}

trait GDouble extends Gradient[Double] {
  override val GCDS: Stream[GCD] = new GCD {
    override val gc: LangTerm[Double => Double] = ltl.I(di)

    override val gd: LangTerm[Double => Double] = ltl.I(di)
  } #:: Stream.Empty

  override implicit def GInfo: LangInfoG[Double] = ltl.doubleInfo

  override def constG: LangTerm[Double] = ltl.litD(0)

  def lift: LangTerm[Double => Double] =>
    LangTerm[((Double, Double)) => Double] =>
      LangTerm[((Double, Double)) => (Double, Double)] = origf => gradf => {
    ltl.S__(ltl.B__(ltl.mkProd[Double, Double])(ltl.B__(origf)(ltl.zro[Double, Double])))(gradf)
  }

  def lift2: LangTerm[Double => Double => Double] =>
    LangTerm[((Double, Double)) => ((Double, Double)) => Double] =>
      LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] = origf => gradf => {
    ltl.curry_(ltl.S__(
      ltl.B__(ltl.mkProd[Double, Double])(
        ltl.uncurry_(ltl.C_(ltl.B__(ltl.C_(ltl.B__(origf)(ltl.zro[Double, Double])))(ltl.zro[Double, Double])))))(
      ltl.uncurry_(gradf)))
  }

  override def plus: LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] =
    lift2(ltl.plusD)(ltl.B__(ltl.C_(ltl.B__(ltl.plusD)(ltl.fst[Double, Double])))(ltl.fst[Double, Double]))

  override def mult: LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] =
    lift2(ltl.multD)({
      val l = NextLang.apply[LangInfoG, LangTerm, (Double, Double)](ltl, ltl.prodInfo(ltl.doubleInfo, ltl.doubleInfo))
      val r = NextLang.apply[LangInfoG, l.repr, (Double, Double)](l, ltl.prodInfo(ltl.doubleInfo, ltl.doubleInfo))
      l.collapse(r.collapse(
        r.plusD__(r.multD__(r.zro_(r.rconv(l.in)))(r.fst_(r.in)))(r.multD__(r.zro_(r.in))(r.fst_(r.rconv(l.in))))))
    })

  override def div: LangTerm[((Double, Double)) => ((Double, Double)) => (Double, Double)] =
    lift2(ltl.divD)({
      val l = NextLang.apply[LangInfoG, LangTerm, (Double, Double)](ltl, ltl.prodInfo(ltl.doubleInfo, ltl.doubleInfo))
      val r = NextLang.apply[LangInfoG, l.repr, (Double, Double)](l, ltl.prodInfo(ltl.doubleInfo, ltl.doubleInfo))
      l.collapse(r.collapse(
        r.plusD__(
          r.divD__(r.fst_(r.rconv(l.in)))(r.zro_(r.in)))(
          r.multD__(r.litD(-1))(r.divD__(r.zro_(r.rconv(l.in)))(r.multD__(r.fst_(r.in))(r.fst_(r.in)))))))
    })

  override def sig: LangTerm[((Double, Double)) => (Double, Double)] = {
    val n = NextLang.apply[LangInfoG, LangTerm, (Double, Double)](ltl, ltl.prodInfo(ltl.doubleInfo, ltl.doubleInfo))
    n.collapse(n.Let__(n.sigD_(n.zro_(n.in)))({
      val m = NextLang.apply[LangInfoG, n.repr, Double](n, ltl.doubleInfo)
      m.collapse(m.mkProd__(m.in)(
        m.multD__(m.fst_(m.rconv(n.in)))(
          m.multD__(m.in)(
            m.plusD__(m.litD(1))(m.multD__(m.litD(-1))(m.in))))))
    }))
  }

  override def exp: LangTerm[((Double, Double)) => (Double, Double)] =
    lift(ltl.expD)(
      ltl.S__[(Double, Double), Double, Double](
        ltl.B__(ltl.multD)(ltl.zro[Double, Double]))(
        ltl.fst[Double, Double]))
}

object GDouble extends GDouble