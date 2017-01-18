package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Bool.ADEvalBool
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang, NextLang}
import com.thoughtworks.DDF.Product.ADEvalProd
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}

trait ADEvalDoubleMin extends
  DoubleMin[ADEvalCase, ADEval] with
  ADEvalArr with
  ADEvalBool with
  ADEvalProd {
  implicit val ti = base.topInfo

  implicit val di = base.doubleInfo

  implicit val bi = base.boolInfo

  implicit def pi[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = base.prodInfo(ai, bi)

  implicit def ioi[A, B](implicit ai: LangInfoG[A]) = base.IOInfo(ai)

  implicit def ai[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = base.aInfo(ai, bi)

  override def ltD =
    new ADEval[scala.Double => scala.Double => Boolean] {
      override val fec = aInfo(doubleInfo, aInfo(doubleInfo, boolInfo))

      override def term[G: Gradient] = {
        implicit val GI = implicitly[Gradient[G]].GInfo
        base.C_(base.B__(
          base.C_(base.B__(base.ltD)(base.zro(base.doubleInfo, GI)))
        )(base.zro(base.doubleInfo, implicitly[Gradient[G]].GInfo)))
      }
    }

  override def multD = new ADEval[scala.Double => scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override def term[G: Gradient] = {
      implicit val gi = implicitly[Gradient[G]].GInfo
      implicit val l = NextLang.apply[LangInfoG, LangTerm, (scala.Double, G)]
      val r = NextLang.apply[LangInfoG, l.repr, (scala.Double, G)]
      l.collapse(r.collapse(r.mkProd__(
        r.multD__(r.rconv(l.zro_(l.in)))(r.zro_(r.in)))(
        r.app(r.app(r.rconv(l.rconv(implicitly[Gradient[G]].plus)))(
          r.app(r.app(r.rconv(l.rconv(implicitly[Gradient[G]].mult)))(r.rconv(l.zro_(l.in))))(r.fst_(r.in))))(
          r.app(r.app(r.rconv(l.rconv(implicitly[Gradient[G]].mult)))(r.zro_(r.in)))(r.rconv(l.fst_(l.in)))))))
    }
  }

  override def expD = new ADEval[scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, doubleInfo)

    override def term[G: Gradient] = {
      implicit val gi = implicitly[Gradient[G]].GInfo
      implicit val p = NextLang.apply[LangInfoG, LangTerm, (scala.Double, G)]
      p.collapse(p.Let__(p.expD_(p.zro_(p.in)))({
        implicit val ret = NextLang.apply[LangInfoG, p.repr, scala.Double]
        ret.collapse(ret.mkProd__(
          ret.in)(
          ret.app(ret.app(ret.rconv(p.rconv(implicitly[Gradient[G]].mult)))(ret.in))(ret.rconv(p.fst_(p.in)))))
      }))
    }
  }

  override def recipD = new ADEval[scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, doubleInfo)

    override def term[G: Gradient] = {
      implicit val gi = implicitly[Gradient[G]].GInfo
      implicit val p = NextLang.apply[LangInfoG, LangTerm, (scala.Double, G)]
      p.collapse(p.mkProd__(p.recipD_(p.zro_(p.in)))(
        p.app(p.app(p.rconv(implicitly[Gradient[G]].mult))(
          p.multD__(p.litD(-1))(p.recipD_(p.multD__(p.zro_(p.in))(p.zro_(p.in))))))(
          p.fst_(p.in))))
    }
  }

  override def sigD = new ADEval[scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, doubleInfo)

    override def term[G: Gradient] = {
      implicit val gi = implicitly[Gradient[G]].GInfo
      implicit val p = NextLang.apply[LangInfoG, LangTerm, (scala.Double, G)]
      p.collapse(p.Let__(p.recipD_(p.plusD__(p.litD(1))(p.expD_(p.multD__(p.litD(-1))(p.zro_(p.in))))))({
        implicit val ret = NextLang.apply[LangInfoG, p.repr, scala.Double]
        ret.collapse(ret.mkProd__(
          ret.in)(
          ret.app(ret.app(
            ret.rconv(p.rconv(implicitly[Gradient[G]].mult)))(
            ret.multD__(ret.in)(ret.plusD__(ret.litD(1))(ret.multD__(ret.litD(-1))(ret.in)))))(
            ret.rconv(p.fst_(p.in)))))
      }))
    }
  }

  override def plusD = new ADEval[scala.Double => scala.Double => scala.Double] {
    override val fec = aInfo(doubleInfo, aInfo(doubleInfo, doubleInfo))

    override def term[G: Gradient] = {
      implicit val gi = implicitly[Gradient[G]].GInfo
      implicit val l = NextLang.apply[LangInfoG, LangTerm, (scala.Double, G)]
      val r = NextLang.apply[LangInfoG, l.repr, (scala.Double, G)]
      l.collapse(r.collapse(r.mkProd__(
        r.plusD__(r.rconv(l.zro_(l.in)))(r.zro_(r.in)))(
        r.app(r.app(r.rconv(l.rconv(implicitly[Gradient[G]].plus)))(r.rconv(l.fst_(l.in))))(r.fst_(r.in)))))
    }
  }

  override def litD: scala.Double => ADEval[scala.Double] = d =>
    new ADEval[scala.Double] {
      override val fec = doubleInfo

      override def term[G: Gradient] = {
        implicit val gi = implicitly[Gradient[G]].GInfo
        base.mkProd__(base.litD(d))(implicitly[Gradient[G]].constG)
      }
    }

  override implicit def doubleInfo: ADEvalCase.Aux[scala.Double, Lambda[X => (scala.Double, X)]] =
    new ADEvalCase[scala.Double] {
      override type WithGrad[G] = (scala.Double, G)

      override val tm = new ADEvalMatch[scala.Double] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()

      override def wgi[G: Gradient]: LangInfoG[(scala.Double, G)] =
        base.prodInfo(base.doubleInfo, implicitly[Gradient[G]].GInfo)
    }
}

object ADEvalDoubleMin extends ADEvalDoubleMin