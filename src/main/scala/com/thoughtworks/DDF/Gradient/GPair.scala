package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}

trait GPair[A, B] extends Gradient[(A, B)] {
  def combine[C]: Stream[C] => Stream[C] => Stream[C] = l => r => (l, r) match {
    case (Stream.Empty, _) => r
    case (_, Stream.Empty) => l
    case (lh #:: lt, rh #:: rt) => lh #:: (rh #:: combine(lt)(rt))
  }

  implicit val AG: Gradient[A]

  implicit val BG: Gradient[B]

  override implicit def GInfo: LangInfoG[(A, B)] = ltl.prodInfo(AG.GInfo, BG.GInfo)

  override def constG: LangTerm[(A, B)] = ltl.mkProd__(AG.constG)(BG.constG)

  implicit val ai = AG.GInfo

  implicit val bi = BG.GInfo

  override val GCDS: Stream[GCD] =
    combine(AG.GCDS.map(x => new GCD {
      override val gc: LangTerm[Double => (A, B)] = ltl.B__(ltl.C__(ltl.mkProd[A, B])(BG.constG))(x.gc)
      override val gd: LangTerm[((A, B)) => Double] = ltl.B__(x.gd)(ltl.zro[A, B])
    }))(BG.GCDS.map(x => new GCD {
      override val gc: LangTerm[Double => (A, B)] = ltl.B__(ltl.mkProd_[A, B](AG.constG))(x.gc)
      override val gd: LangTerm[((A, B)) => Double] = ltl.B__(x.gd)(ltl.fst[A, B])
    }))

  import ltl.{prodInfo => lpinfo}

  def lift:
  LangTerm[((Double, A)) => (Double, A)] =>
    LangTerm[((Double, B)) => (Double, B)] =>
      LangTerm[((Double, (A, B))) => (Double, (A, B))] = af => bf => {
    val nLang = NextLang.apply[LangInfoG, LangTerm, (Double, (A, B))](ltl, lpinfo(di, lpinfo(ai, bi)))
    import nLang._
    val a = app(rconv(af))(mkProd__(zro_(in))(zro_(fst_(in))))
    val b = app(rconv(bf))(mkProd__(zro_(in))(fst_(fst_(in))))
    collapse(Let__(a)(
      Let__[(Double, B), ((Double, A)) => (Double, (A, B))](b)({
        val nNLang = NextLang.apply[LangInfoG, nLang.repr, (Double, B)](nLang, prodInfo(di, bi))
        val nNNLang = NextLang.apply[LangInfoG, nNLang.repr, (Double, A)](nNLang, prodInfo(di, ai))
        nNLang.collapse(nNNLang.collapse(nNNLang.mkProd__(nNNLang.zro_(nNNLang.in))(
          nNNLang.mkProd__(nNNLang.fst_(nNNLang.in))(nNNLang.fst_(nNNLang.rconv(nNLang.in))))))
      })))
  }

  def lift2:
  LangTerm[((Double, A)) => ((Double, A)) => (Double, A)] =>
    LangTerm[((Double, B)) => ((Double, B)) => (Double, B)] =>
      LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] = af => bf => {
    val nLang = NextLang.apply[LangInfoG, LangTerm, (Double, (A, B))](ltl, lpinfo(di, lpinfo(ai, bi)))
    val nNLang = NextLang.apply[LangInfoG, nLang.repr, (Double, (A, B))](nLang, lpinfo(di, lpinfo(ai, bi)))
    import nNLang._
    val a = app(app(rconv(nLang.rconv(af)))(
      mkProd__(zro_(rconv(nLang.in)))(zro_(fst_(rconv(nLang.in))))))(mkProd__(zro_(in))(zro_(fst_(in))))
    val b = app(app(rconv(nLang.rconv(bf)))(
      mkProd__(zro_(rconv(nLang.in)))(fst_(fst_(rconv(nLang.in))))))(mkProd__(zro_(in))(fst_(fst_(in))))
    nLang.collapse(collapse(Let__(a)(
      Let__[(Double, B), ((Double, A)) => (Double, (A, B))](b)({
        val nNNLang = NextLang.apply[LangInfoG, nNLang.repr, (Double, B)](nNLang, prodInfo(di, bi))
        val nNNNLang = NextLang.apply[LangInfoG, nNNLang.repr, (Double, A)](nNNLang, prodInfo(di, ai))
        nNNLang.collapse(nNNNLang.collapse(nNNNLang.mkProd__(nNNNLang.zro_(nNNNLang.in))(
          nNNNLang.mkProd__(nNNNLang.fst_(nNNNLang.in))(nNNNLang.fst_(nNNNLang.rconv(nNNLang.in))))))
      }))))
  }

  override def plus: LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] =
    lift2(AG.plus)(BG.plus)

  override def mult: LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] =
    lift2(AG.mult)(BG.mult)

  override def div: LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] =
    lift2(AG.div)(BG.div)

  override def sig: LangTerm[((Double, (A, B))) => (Double, (A, B))] = lift(AG.sig)(BG.sig)

  override def exp: LangTerm[((Double, (A, B))) => (Double, (A, B))] = lift(AG.exp)(BG.exp)
}

object GPair {
  implicit def apply[A, B](implicit ag: Gradient[A], bg: Gradient[B]) = new GPair[A, B] {
    override implicit val AG: Gradient[A] = ag

    override implicit val BG: Gradient[B] = bg
  }
}