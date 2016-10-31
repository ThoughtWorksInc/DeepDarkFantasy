package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.InterLangTerm

trait EvalOMatch[X] extends TypeMatch[EvalOMatch, X]

object EvalOMatch {
  type Aux[X, XM] = EvalOMatch[X] {type ret = XM}
}

trait EvalO[X] extends TypeCase[EvalOMatch, X] {
  def l: InterLangTerm[X]
}
