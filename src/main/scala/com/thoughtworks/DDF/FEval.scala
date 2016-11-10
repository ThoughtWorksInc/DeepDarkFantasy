package com.thoughtworks.DDF

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

import scalaz.Leibniz._

trait FEvalMatch[X] extends TypeMatch[FEvalMatch, X]

trait FEvalCase[X] extends TypeCase[FEvalMatch, X] {
  type WithGrad[_]

  def unique[G](r: FEvalCase[X]): WithGrad[G] === r.WithGrad[G] =
  /*enforced by user*/ force[Nothing, Any, WithGrad[G], r.WithGrad[G]]

  def wgi[G: Gradient]: LangInfoG[WithGrad[G]]
}

object FEvalCase {
  type Aux[X, XM[_]] = FEvalCase[X] {type WithGrad[Y] = XM[Y]}
}

trait FEval[X] {
  val fec: FEvalCase[X]

  def term[G: Gradient]: LangTerm[fec.WithGrad[G]]

  def get[G: Gradient](f: FEvalCase[X]): LangTerm[f.WithGrad[G]] = fec.unique[G](f).subst(term)
}