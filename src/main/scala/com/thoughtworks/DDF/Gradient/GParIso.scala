package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}
import com.thoughtworks.DDF.{Prop, SemEq}

trait GParIso[A, B] extends Gradient[B] {
  implicit def ga: Gradient[A]

  def ab: LangTerm[A => B]

  def ba: LangTerm[B => A]

  def law(x: SemEq[A => A]): Prop = x.semEq(ltl.B__(ba)(ab))(ltl.I(ga.GInfo))

  override val GCDS: Stream[GCD] = ga.GCDS.map(x => new GCD {
    override val gc: LangTerm[Double => B] = ltl.B__(ab)(x.gc)
    override val gd: LangTerm[B => Double] = ltl.B__(x.gd)(ba)
  })

  override implicit def GInfo: LangInfoG[B] = ltl.rngInfo(ltl.reprInfo(ab))

  override def constG: LangTerm[B] = ltl.app(ab)(ga.constG)

  override def mult: LangTerm[Double => B => B] = ???

  override def plus: LangTerm[B => B => B] =
    ltl.B__[B, B => A, B => B](ltl.B_(ab))(ltl.C_(ltl.B__(ltl.C_(ltl.B__(ga.plus)(ba)))(ba)))
}

object GParIso {
  def apply[A, B](aconv: LangTerm[A => B])(bconv: LangTerm[B => A])(grad: Gradient[A]) = new GParIso[A, B] {
    override val ab: LangTerm[A => B] = aconv

    override val ba: LangTerm[B => A] = bconv

    override val ga: Gradient[A] = grad
  }
}