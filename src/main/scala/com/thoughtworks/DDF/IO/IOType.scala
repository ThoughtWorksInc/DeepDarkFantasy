package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.LangBase.LangBase

trait IOType extends LangBase {
  type IO[A <: Type] <: Type

  implicit def IOK[A <: Type: Kind]: Kind[IO[A]]
}
