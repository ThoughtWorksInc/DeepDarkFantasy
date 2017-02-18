package com.thoughtworks.DDF.LangBase

import com.thoughtworks.DDF.NoInfo

trait SimpleLangBase extends LangBase {
  override def reprInfo[A] = _ => NoInfo()
}

object SimpleLangBase {
  implicit def apply[Repr[_]] = new SimpleLangBase[Repr] {}
}