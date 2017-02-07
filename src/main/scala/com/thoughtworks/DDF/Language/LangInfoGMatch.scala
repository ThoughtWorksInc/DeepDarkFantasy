package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.RecursiveInfoMatch

trait LangInfoGMatch[X] extends RecursiveInfoMatch[LangInfoGMatch, X]

object LangInfoGMatch {
  type Aux[X, Y] = LangInfoGMatch[X] { type ret = Y }
}