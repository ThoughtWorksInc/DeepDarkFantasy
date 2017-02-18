package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.LangBase.LangBase

trait ListType extends LangBase {
  type List[+A <: Type] <: Type

  implicit def ListK[A <: Type: Kind]: Kind[List[A]]
}
