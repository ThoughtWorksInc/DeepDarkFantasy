package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.LangBase.LangBase

trait DoubleType extends LangBase {
  type Double <: Type

  implicit def DoubleK: Kind[Double]
}
