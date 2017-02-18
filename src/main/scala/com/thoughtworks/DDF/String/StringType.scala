package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.LangBase.LangBase

trait StringType extends LangBase {
  type String <: Type

  implicit def StringK: Kind[String]
}
