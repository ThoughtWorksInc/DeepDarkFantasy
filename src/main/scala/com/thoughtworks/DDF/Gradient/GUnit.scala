package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GUnit extends Gradient[Unit] {
  override implicit def GInfo: LangInfoG[Unit] = ltl.topInfo

  override def constG: LangTerm[Unit] = ltl.mkTop

  def lift: LangTerm[Double => Double] => LangTerm[((Double, Unit)) => (Double, Unit)] = f =>
    ltl.B__(
      ltl.C__[Double, Unit, (Double, Unit)](ltl.mkProd[Double, Unit])(ltl.mkTop))(
      ltl.B__(f)(ltl.zro[Double, Unit]))

  def lift2:
  LangTerm[Double => Double => Double] =>
    LangTerm[((Double, Unit)) => ((Double, Unit)) => (Double, Unit)] = f =>
    ltl.B__[((Double, Unit)), ((Double, Unit)) => Double, ((Double, Unit)) => (Double, Unit)](
      ltl.B_[(Double, Unit), Double, (Double, Unit)](
        ltl.C__[Double, Unit, (Double, Unit)](ltl.mkProd[Double, Unit])(ltl.mkTop)))(ltl.C_(ltl.B__(
      ltl.C_(ltl.B__(f)(ltl.zro[Double, Unit])))(
      ltl.zro[Double, Unit])))

  override def plus: LangTerm[((Double, Unit)) => ((Double, Unit)) => (Double, Unit)] = lift2(ltl.plusD)

  override def mult: LangTerm[((Double, Unit)) => ((Double, Unit)) => (Double, Unit)] = lift2(ltl.multD)

  override def div: LangTerm[((Double, Unit)) => ((Double, Unit)) => (Double, Unit)] = lift2(ltl.divD)

  override def sig: LangTerm[((Double, Unit)) => (Double, Unit)] = lift(ltl.sigD)

  override def exp: LangTerm[((Double, Unit)) => (Double, Unit)] = lift(ltl.expD)
}

object GUnit extends GUnit