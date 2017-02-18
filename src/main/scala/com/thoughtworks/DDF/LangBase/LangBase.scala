package com.thoughtworks.DDF.LangBase

trait LangBase {
  type Kind[_ <: Type]

  type Type
}
