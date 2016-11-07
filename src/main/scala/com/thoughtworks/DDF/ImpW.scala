package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.LangTerm

trait ImpW[X] {
  type Weight

  implicit val GW: Gradient[Weight]

  val term: LangTerm[Weight => X]
}
