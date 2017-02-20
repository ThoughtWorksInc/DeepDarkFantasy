package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.LangBase.LangBase

trait StreamType extends LangBase {
  type Stream[+A <: Type] <: Type

  implicit def StreamK[A <: Type: Kind]: Kind[Stream[A]]
}
