package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.{EvalMInterLang, InterLangTermInterLang}

trait EvalOBase {
  val ltl = InterLangTermInterLang

  def eval[X]: EvalO[X] => X = x => x.l.apply[NoInfo, Lambda[X => X]](EvalMInterLang)
}
