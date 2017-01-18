package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.TypeMatch

trait LangInfoGMatch[X] extends TypeMatch[LangInfoGMatch, X]

object LangInfoGMatch {
  type Aux[X, Y] = LangInfoGMatch[X] { type ret = Y }
}