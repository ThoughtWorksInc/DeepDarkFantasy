package com.thoughtworks.DDF

trait EvalOMatch[X] extends TypeMatch[EvalOMatch, X]

object EvalOMatch {
  type Aux[X, XM] = EvalOMatch[X] {type ret = XM}
}