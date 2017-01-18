package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.NoInfo

trait SimpleBool[Repr[_]] extends BoolInfo[NoInfo, Repr] {
  override implicit def boolInfo: NoInfo[Boolean] = NoInfo()
}

object SimpleBool {
  implicit def apply[Repr[_]] = new SimpleBool[Repr] {}
}
