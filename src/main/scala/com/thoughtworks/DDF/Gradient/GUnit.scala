package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GUnit extends Gradient[Unit] {
  override implicit def GInfo: LangInfoG[Unit] = ltl.topInfo

  override def constG: LangTerm[Unit] = ltl.mkTop

  override val GCDS: Stream[GCD] = Stream.Empty

  override def mult: LangTerm[Double => Unit => Unit] = ltl.C_(ltl.K[Unit, Double])

  override def plus: LangTerm[Unit => Unit => Unit] = ltl.K[Unit, Unit]
}

object GUnit extends GUnit