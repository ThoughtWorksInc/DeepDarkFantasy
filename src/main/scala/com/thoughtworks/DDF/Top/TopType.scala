package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.LangBase.LangBase

trait TopType extends LangBase {
  type Top <: Type

  implicit def TopK: Kind[Top]
}
