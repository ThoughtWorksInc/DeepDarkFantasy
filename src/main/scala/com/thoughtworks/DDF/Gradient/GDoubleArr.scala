package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.LangTerm

trait GDoubleArr[G] extends Gradient[Double => G] with GParIso[G, Double => G] {
  override def ab: LangTerm[G => Double => G] = ltl.C_(ga.mult)

  override def ba: LangTerm[(Double => G) => G] = ltl.Let_[Double, G](ltl.litD(1))(ga.GInfo)
}

object GDoubleArr {
  implicit def apply[G](implicit g: Gradient[G]): Gradient[Double => G] = new GDoubleArr[G] {
    override implicit def ga: Gradient[G] = g
  }
}