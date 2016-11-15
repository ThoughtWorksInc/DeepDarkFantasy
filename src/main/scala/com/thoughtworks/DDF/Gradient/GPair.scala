package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang, NextLang}
import com.thoughtworks.DDF.Util

trait GPair[A, B] extends Gradient[(A, B)] {
  implicit val AG: Gradient[A]

  implicit val BG: Gradient[B]

  override implicit def GInfo: LangInfoG[(A, B)] = ltl.prodInfo(AG.GInfo, BG.GInfo)

  override def constG: LangTerm[(A, B)] = ltl.mkProd__(AG.constG)(BG.constG)

  implicit val ai = AG.GInfo

  implicit val bi = BG.GInfo

  override val GCDS: Stream[GCD] =
    Util.combine(AG.GCDS.map(x => new GCD {
      override val gc: LangTerm[Double => (A, B)] = ltl.B__(ltl.C__(ltl.mkProd[A, B])(BG.constG))(x.gc)
      override val gd: LangTerm[((A, B)) => Double] = ltl.B__(x.gd)(ltl.zro[A, B])
    }))(BG.GCDS.map(x => new GCD {
      override val gc: LangTerm[Double => (A, B)] = ltl.B__(ltl.mkProd_[A, B](AG.constG))(x.gc)
      override val gd: LangTerm[((A, B)) => Double] = ltl.B__(x.gd)(ltl.fst[A, B])
    }))

  override def mult: LangTerm[Double => ((A, B)) => (A, B)] =
    ltl.C_[(A, B), Double, (A, B)]({
      val p = NextLang.apply[LangInfoG, LangTerm, (A, B)]
      p.collapse(
        p.S__[Double, B, (A, B)](
          p.B__(p.mkProd[A, B])(p.B__(p.Let_[A, A](p.zro_(p.in)): p.repr[(A => A) => A])(p.rconv(AG.mult))))(
          p.B__(p.Let_[B, B](p.fst_(p.in)))(p.rconv(BG.mult))))
    })

  override def plus: LangTerm[((A, B)) => ((A, B)) => (A, B)] = {
    implicit val l = NextLang.apply[LangInfoG, LangTerm, (A, B)]
    val r = NextLang.apply[LangInfoG, l.repr, (A, B)]
    l.collapse(r.collapse(r.mkProd__(
      r.app(r.app(r.rconv(l.rconv(AG.plus)))(r.zro_(r.rconv(l.in))))(r.zro_(r.in)))(
      r.app(r.app(r.rconv(l.rconv(BG.plus)))(r.fst_(r.rconv(l.in))))(r.fst_(r.in)))))
  }
}

object GPair {
  implicit def apply[A, B](implicit ag: Gradient[A], bg: Gradient[B]) = new GPair[A, B] {
    override implicit val AG: Gradient[A] = ag

    override implicit val BG: Gradient[B] = bg
  }
}