package com.thoughtworks.DDF.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, NextLang}

trait GDouble extends Gradient[Double] {
  override val GCDS: Stream[GCD] = new GCD {
    override val gc: LangTerm[Double => Double] = ltl.I(di)

    override val gd: LangTerm[Double => Double] = ltl.I(di)
  } #:: Stream.Empty

  override implicit def GInfo: LangInfoG[Double] = ltl.doubleInfo

  override def constG: LangTerm[Double] = ltl.litD(0)
}

object GDouble extends GDouble