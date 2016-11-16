package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}
import com.thoughtworks.DDF.Util

trait GPairArr[A, B, C] extends Gradient[((A, B)) => C] {
  implicit def gac: Gradient[A => C]

  implicit def gbc: Gradient[B => C]

  implicit def ga: Gradient[A]

  implicit def gb: Gradient[B]

  implicit def gc: Gradient[C]

  implicit val ai = ga.GInfo

  implicit val bi = gb.GInfo

  implicit val ci = gc.GInfo

  val abcac: LangTerm[(((A, B)) => C) => A => C] = ltl.C__(ltl.B[A, (A, B), C])(ltl.C__(ltl.mkProd[A, B])(gb.constG))

  val abcbc: LangTerm[(((A, B)) => C) => B => C] = ltl.C__(ltl.B[B, (A, B), C])(ltl.mkProd_[A, B](ga.constG))

  override val GCDS: Stream[GCD] = Util.combine(
    gac.GCDS.map(x => new GCD {
      override def gc: LangTerm[Double => ((A, B)) => C] = ltl.B__(ltl.C__(ltl.B[(A, B), A, C])(ltl.zro[A, B]))(x.gc)
      override def gd: LangTerm[(((A, B)) => C) => Double] = ltl.B__[((A, B)) => C, A => C, Double](x.gd)(abcac)
    }))(gbc.GCDS.map(x => new GCD {
    override def gc: LangTerm[Double => ((A, B)) => C] = ltl.B__(ltl.C__(ltl.B[(A, B), B, C])(ltl.fst[A, B]))(x.gc)
    override def gd: LangTerm[(((A, B)) => C) => Double] = ltl.B__[((A, B)) => C, B => C, Double](x.gd)(abcbc)
  }))

  override implicit def GInfo: LangInfoG[((A, B)) => C] = ltl.aInfo

  override def constG: LangTerm[((A, B)) => C] = ltl.K_(gc.constG)

  override val mult: LangTerm[Double => (((A, B)) => C) => ((A, B)) => C] = {
    implicit val d = NextLang.apply[LangInfoG, LangTerm, Double]
    val f = NextLang.apply[LangInfoG, d.repr, (((A, B)) => C)]
    d.collapse(f.collapse(f.B__(f.in)(f.rconv(d.app(d.rconv(GPair.apply(ga, gb).mult))(d.in)))))
  }

  override val plus: LangTerm[(((A, B)) => C) => (((A, B)) => C) => ((A, B)) => C] = {
    implicit val l = NextLang.apply[LangInfoG, LangTerm, ((A, B)) => C]
    implicit val r = NextLang.apply[LangInfoG, l.repr, ((A, B)) => C]
    val p = NextLang.apply[LangInfoG, r.repr, (A, B)]
    def rconv[X](x: LangTerm[X]) = p.rconv(r.rconv(l.rconv(x)))
    l.collapse(r.collapse(p.collapse(
      p.app(p.app(rconv(gc.plus))(
        p.app(p.app(p.app(rconv(gac.plus))(
          p.app(rconv(abcac))(p.rconv(r.rconv(l.in)))))(
          p.app(rconv(abcac))(p.rconv(r.in))))(p.zro_(p.in))))(
        p.app(p.app(p.app(rconv(gbc.plus))(
          p.app(rconv(abcbc))(p.rconv(r.rconv(l.in)))))(
          p.app(rconv(abcbc))(p.rconv(r.in))))(p.fst_(p.in))))))
  }
}

object GPairArr {
  implicit def apply[A, B, C](implicit
                              GAC: Gradient[A => C],
                              GBC: Gradient[B => C],
                              GA: Gradient[A],
                              GB: Gradient[B],
                              GC: Gradient[C]) = new GPairArr[A, B, C] {
    override implicit def gac: Gradient[A => C] = GAC

    override implicit def gbc: Gradient[B => C] = GBC

    override implicit def ga: Gradient[A] = GA

    override implicit def gb: Gradient[B] = GB

    override implicit def gc: Gradient[C] = GC
  }
}