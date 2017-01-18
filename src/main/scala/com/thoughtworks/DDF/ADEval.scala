package com.thoughtworks.DDF

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

import scalaz.Leibniz._

trait ADEval[X] {
  val fec: ADEvalCase[X]

  def term[G: Gradient]: LangTerm[fec.WithGrad[G]]

  def get[G: Gradient](f: ADEvalCase[X]): LangTerm[f.WithGrad[G]] = fec.unique[G](f).subst(term)
}