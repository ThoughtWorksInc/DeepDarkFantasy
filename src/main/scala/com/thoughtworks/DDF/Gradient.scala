package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait Gradient[G] {
  implicit def GInfo: LangInfoG[G]

  def constG: LangTerm[G]
}
