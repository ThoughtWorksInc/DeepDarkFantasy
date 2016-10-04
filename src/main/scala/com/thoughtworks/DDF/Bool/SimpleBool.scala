package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.NoInfo

trait SimpleBool[Repr[_]] extends BoolInfo[NoInfo, Repr] with SimpleInfoBase[Repr] {
  override implicit def BoolInfo: NoInfo[Boolean] = NoInfo()
}

object SimpleBool {
  implicit def apply[Repr[_]] = new SimpleBool[Repr] {}
}
