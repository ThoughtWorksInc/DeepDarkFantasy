package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}

trait GArrPair[A, B, C] extends Gradient[A => (B, C)] {
  implicit val arrBG: Gradient[A => B]

  implicit val arrCG: Gradient[A => C]

  import ltl.{prodInfo => lpinfo}

  implicit val di = ltl.doubleInfo

  implicit val ai = ltl.domInfo(arrBG.GInfo)

  implicit val bi = ltl.rngInfo(arrBG.GInfo)

  implicit val ci = ltl.rngInfo(arrCG.GInfo)

  override implicit def GInfo: LangInfoG[A => (B, C)] = ltl.aInfo[A, (B, C)]

  override def constG: LangTerm[A => (B, C)] =
    ltl.S__(ltl.B__(ltl.mkProd[B, C])(arrBG.constG))(arrCG.constG)

  def lift:
  LangTerm[((Double, A => B)) => (Double, A => B)] =>
    LangTerm[((Double, A => C)) => (Double, A => C)] =>
      LangTerm[((Double, A => (B, C))) => (Double, A => (B, C))] = af => bf => {
    val nLang = NextLang.apply[LangInfoG, LangTerm, (Double, A => (B, C))](ltl, lpinfo(di, GInfo))
    import nLang._
    val a = app(rconv(af))(mkProd__(zro_(in))(B__(zro[B, C])(fst_(in))))
    val b = app(rconv(bf))(mkProd__(zro_(in))(B__(fst[B, C])(fst_(in))))
    collapse(Let__(a)(
      Let__[(Double, A => C), ((Double, A => B)) => (Double, A => (B, C))](b)({
        val nNLang = NextLang.apply[LangInfoG, nLang.repr, (Double, A => C)](nLang, prodInfo(di, aInfo(ai, ci)))
        val nNNLang = NextLang.apply[LangInfoG, nNLang.repr, (Double, A => B)](nNLang, prodInfo(di, aInfo(ai, bi)))
        nNLang.collapse(nNNLang.collapse(nNNLang.mkProd__(nNNLang.zro_(nNNLang.in))(nNNLang.S__(
          nNNLang.B__(nNNLang.mkProd[B, C])(nNNLang.fst_(nNNLang.in)))(
          nNNLang.fst_(nNNLang.rconv(nNLang.in))))))
      })))
  }

  def lift2:
  LangTerm[((Double, A => B)) => ((Double, A => B)) => (Double, A => B)] =>
    LangTerm[((Double, A => C)) => ((Double, A => C)) => (Double, A => C)] =>
      LangTerm[((Double, A => (B, C))) => ((Double, A => (B, C))) => (Double, A => (B, C))] = af => bf => {
    val nLang = NextLang.apply[LangInfoG, LangTerm, (Double, A => (B, C))](ltl, ltl.prodInfo(di, GInfo))
    val nNLang = NextLang.apply[LangInfoG, nLang.repr, (Double, A => (B, C))](nLang, ltl.prodInfo(di, GInfo))
    import nNLang._
    val a = app(app(rconv(nLang.rconv(af)))(
      mkProd__(zro_(rconv(nLang.in)))(B__(zro[B, C])(fst_(rconv(nLang.in))))))(
      mkProd__(zro_(in))(B__(zro[B, C])(fst_(in))))
    val b = app(app(rconv(nLang.rconv(bf)))(
      mkProd__(zro_(rconv(nLang.in)))(B__(fst[B, C])(fst_(rconv(nLang.in))))))(
      mkProd__(zro_(in))(B__(fst[B, C])(fst_(in))))
    nLang.collapse(collapse(Let__(a)(Let__[(Double, A => C), ((Double, A => B)) => (Double, A => (B, C))](b)({
      val nNNLang = NextLang.apply[LangInfoG, nNLang.repr, (Double, A => C)](nNLang, prodInfo(di, aInfo(ai, ci)))
      val nNNNLang = NextLang.apply[LangInfoG, nNNLang.repr, (Double, A => B)](nNNLang, prodInfo(di, aInfo(ai, bi)))
      nNNLang.collapse(nNNNLang.collapse(nNNNLang.mkProd__(nNNNLang.zro_(nNNNLang.in))(nNNNLang.S__(
        nNNNLang.B__(nNNNLang.mkProd[B, C])(nNNNLang.fst_(nNNNLang.in)))(
        nNNNLang.fst_(nNNNLang.rconv(nNNLang.in))))))
    }))))
  }

  override def plus: LangTerm[((Double, A => (B, C))) => ((Double, A => (B, C))) => (Double, A => (B, C))] =
    lift2(arrBG.plus)(arrCG.plus)

  override def mult: LangTerm[((Double, A => (B, C))) => ((Double, A => (B, C))) => (Double, A => (B, C))] =
    lift2(arrBG.mult)(arrCG.mult)

  override def div: LangTerm[((Double, A => (B, C))) => ((Double, A => (B, C))) => (Double, A => (B, C))] =
    lift2(arrBG.div)(arrCG.div)

  override def sig: LangTerm[((Double, A => (B, C))) => (Double, A => (B, C))] = lift(arrBG.sig)(arrCG.sig)

  override def exp: LangTerm[((Double, A => (B, C))) => (Double, A => (B, C))] = lift(arrBG.exp)(arrCG.exp)
}

object GArrPair {
  implicit def apply[A, B, C](implicit ABG: Gradient[A => B], ACG: Gradient[A => C]) = new GArrPair[A, B, C] {
    override implicit val arrBG: Gradient[A => B] = ABG

    override implicit val arrCG: Gradient[A => C] = ACG
  }
}