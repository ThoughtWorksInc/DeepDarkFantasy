package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GArrPair[A, B] extends Gradient[Double => (A, B)] {
  implicit val arrAG: Gradient[Double => A]

  implicit val arrBG: Gradient[Double => B]

  import ltl.prodInfo

  implicit val di = ltl.doubleInfo

  implicit val ai = ltl.rngInfo(arrAG.GInfo)

  implicit val bi = ltl.rngInfo(arrBG.GInfo)
  override implicit def GInfo: LangInfoG[Double => (A, B)] = ltl.aInfo[Double, (A, B)]

  override def constG: LangTerm[(Double) => (A, B)] =
    ltl.S__(ltl.B__(ltl.mkProd[A, B])(arrAG.constG))(arrBG.constG)

  def lift:
  LangTerm[((Double, Double => A)) => (Double, Double => A)] =>
    LangTerm[((Double, Double => B)) => (Double, Double => B)] =>
      LangTerm[Double => Double] =>
        LangTerm[((Double, Double => (A, B))) => (Double, Double => (A, B))] = af => bf => f => ???

  override def plus: LangTerm[((Double, Double => (A, B))) => ((Double, Double => (A, B))) => (Double, Double => (A, B))] = ???

  override def mult: LangTerm[((Double, Double => (A, B))) => ((Double, Double => (A, B))) => (Double, Double => (A, B))] = ???

  override def div: LangTerm[((Double, Double => (A, B))) => ((Double, Double => (A, B))) => (Double, Double => (A, B))] = ???

  override def sig: LangTerm[((Double, Double => (A, B))) => (Double, Double => (A, B))] =
    lift(arrAG.sig)(arrBG.sig)(ltl.sigD)

  override def exp: LangTerm[((Double, Double => (A, B))) => (Double, Double => (A, B))] =
    lift(arrAG.exp)(arrBG.exp)(ltl.expD)
}

object GArrPair {
  implicit def apply[A, B](implicit AAG: Gradient[Double => A], ABG: Gradient[Double => B]) = new GArrPair[A, B] {
    override implicit val arrAG: Gradient[Double => A] = AAG

    override implicit val arrBG: Gradient[Double => B] = ABG
  }
}