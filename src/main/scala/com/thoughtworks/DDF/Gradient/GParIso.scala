package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}
import com.thoughtworks.DDF.{Prop, SemEq}

trait GParIso[A, B] extends Gradient[B] {
  implicit def ga: Gradient[A]

  implicit def gb: Gradient[B]

  implicit def gi = ga.GInfo

  def ab: LangTerm[A => B]

  def ba: LangTerm[B => A]

  def law(x: SemEq[A => A]): Prop = x.semEq(ltl.B__(ba)(ab))(ltl.I(ga.GInfo))

  override val GCDS: Stream[GCD] = ga.GCDS.map(x => new GCD {
    override val gc: LangTerm[Double => B] = ltl.B__(ab)(x.gc)
    override val gd: LangTerm[B => Double] = ltl.B__(x.gd)(ba)
  })

  override def constG: LangTerm[B] = ltl.app(ab)(ga.constG)

  override def mult: LangTerm[Double => B => B] =
    ltl.B__[Double, B => A, B => B](ltl.B_(ab))(ltl.C_(ltl.B__(ltl.C_(ga.mult))(ba)))

  override def plus: LangTerm[B => B => B] =
    ltl.B__[B, B => A, B => B](ltl.B_(ab))(ltl.C_(ltl.B__(ltl.C_(ltl.B__(ga.plus)(ba)))(ba)))

  override implicit def GInfo: LangInfoG[B] = gb.GInfo
}

object GParIso {
  def apply[A: Gradient, B: Gradient](aconv: LangTerm[A => B])(bconv: LangTerm[B => A]) = new GParIso[A, B] {
    override val ab: LangTerm[A => B] = aconv

    override val ba: LangTerm[B => A] = bconv

    override val ga: Gradient[A] = implicitly[Gradient[A]]

    override val gb: Gradient[B] = implicitly[Gradient[B]]
  }
}