package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.LangBase.LangBase

trait BoolType extends LangBase {
  type Bool <: Type

  implicit def BoolK: Kind[Bool]
}
