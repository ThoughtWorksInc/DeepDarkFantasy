package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.LangBase.SimpleLangBase
import com.thoughtworks.DDF.NoInfo

trait SimpleBool[Repr[_]] extends BoolType[NoInfo, Repr] with SimpleLangBase[Repr] {
  override implicit def boolInfo: NoInfo[Boolean] = NoInfo()
}

object SimpleBool {
  implicit def apply[Repr[_]] = new SimpleBool[Repr] {}
}
