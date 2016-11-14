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

  def dadb: LangTerm[((Double, A)) => (Double, B)] = ltl.`>__<`(ltl.I[Double])(ab)

  def dbda: LangTerm[((Double, B)) => (Double, A)] = ltl.`>__<`(ltl.I[Double])(ba)

  def lift: LangTerm[((Double, A)) => (Double, A)] => LangTerm[((Double, B)) => (Double, B)] = f =>
    ltl.B__(dadb)(ltl.B__(f)(dbda))

  def lift2:
  LangTerm[((Double, A)) => ((Double, A)) => (Double, A)] =>
    LangTerm[((Double, B)) => ((Double, B)) => (Double, B)] = f =>
    ltl.curry_(ltl.B__(dadb)(ltl.uncurry_(ltl.C_(ltl.B__(ltl.C_(ltl.B__(f)(dbda)))(dbda)))))

  override def plus: LangTerm[((Double, B)) => ((Double, B)) => (Double, B)] = lift2(ga.plus)

  override def mult: LangTerm[((Double, B)) => ((Double, B)) => (Double, B)] = lift2(ga.mult)

  override def div: LangTerm[((Double, B)) => ((Double, B)) => (Double, B)] = lift2(ga.div)

  override def sig: LangTerm[((Double, B)) => (Double, B)] = lift(ga.sig)

  override def exp: LangTerm[((Double, B)) => (Double, B)] = lift(ga.exp)
}

object GParIso {
  def apply[A, B](aconv: LangTerm[A => B])(bconv: LangTerm[B => A])(grad: Gradient[A]) = new GParIso[A, B] {
    override val ab: LangTerm[A => B] = aconv

    override val ba: LangTerm[B => A] = bconv

    override val ga: Gradient[A] = grad
  }
}