package com.thoughtworks.DDF

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangInfoG

import scalaz.Leibniz._

trait ADEvalCase[X] extends TypeCase[ADEvalMatch, X] {
  type WithGrad[_]

  def unique[G](r: ADEvalCase[X]): WithGrad[G] === r.WithGrad[G] =
  /*enforced by user*/ force[Nothing, Any, WithGrad[G], r.WithGrad[G]]

  def wgi[G: Gradient]: LangInfoG[WithGrad[G]]
}

object ADEvalCase {
  type Aux[X, XM[_]] = ADEvalCase[X] {type WithGrad[Y] = XM[Y]}
}