package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GPair[A, B] extends Gradient[(A, B)] {
  implicit val AG: Gradient[A]

  implicit val BG: Gradient[B]

  override implicit def GInfo: LangInfoG[(A, B)] = ltl.prodInfo(AG.GInfo, BG.GInfo)

  override def constG: LangTerm[(A, B)] = ltl.mkProd__(AG.constG)(BG.constG)

  override def plus: LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] = ???

  override def mult: LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] = ???

  override def div: LangTerm[((Double, (A, B))) => ((Double, (A, B))) => (Double, (A, B))] = ???

  override def sig: LangTerm[((Double, (A, B))) => (Double, (A, B))] = ???

  override def exp: LangTerm[((Double, (A, B))) => (Double, (A, B))] = ???
}

object GPair {
  implicit def apply[A, B](implicit ag: Gradient[A], bg: Gradient[B]) = new GPair[A, B] {
    override implicit val AG: Gradient[A] = ag

    override implicit val BG: Gradient[B] = bg
  }
}