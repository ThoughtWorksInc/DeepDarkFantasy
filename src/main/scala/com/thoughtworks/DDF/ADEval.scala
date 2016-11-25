package com.thoughtworks.DDF

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

import scalaz.Leibniz._

trait ADEvalMatch[X] extends TypeMatch[ADEvalMatch, X]

trait ADEvalCase[X] extends TypeCase[ADEvalMatch, X] {
  type WithGrad[_]

  def unique[G](r: ADEvalCase[X]): WithGrad[G] === r.WithGrad[G] =
  /*enforced by user*/ force[Nothing, Any, WithGrad[G], r.WithGrad[G]]

  def wgi[G: Gradient]: LangInfoG[WithGrad[G]]
}

object ADEvalCase {
  type Aux[X, XM[_]] = ADEvalCase[X] {type WithGrad[Y] = XM[Y]}
}

trait ADEval[X] {
  val fec: ADEvalCase[X]

  def term[G: Gradient]: LangTerm[fec.WithGrad[G]]

  def get[G: Gradient](f: ADEvalCase[X]): LangTerm[f.WithGrad[G]] = fec.unique[G](f).subst(term)
}