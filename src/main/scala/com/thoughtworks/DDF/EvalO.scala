package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.InterLangTerm

trait EvalO[X] extends TypeCase[EvalOMatch[X]] {
  def l: InterLangTerm[X]
}
