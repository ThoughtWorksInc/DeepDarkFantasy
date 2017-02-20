package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.LangBase.LangBase

trait OptionType extends LangBase {
  type Option[+A <: Type] <: Type

  implicit def OptionK[A <: Type: Kind]: Kind[Option[A]]
}
