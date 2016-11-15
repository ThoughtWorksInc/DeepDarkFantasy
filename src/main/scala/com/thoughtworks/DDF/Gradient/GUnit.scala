package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait GUnit extends Gradient[Unit] {
  override implicit def GInfo: LangInfoG[Unit] = ltl.topInfo

  override def constG: LangTerm[Unit] = ltl.mkTop

  override val GCDS: Stream[GCD] = Stream.Empty
}

object GUnit extends GUnit