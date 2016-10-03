package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.InfoB.SimpleInfoB
import com.thoughtworks.DDF.NoInfo

trait SimpleBool[Repr[_]] extends BoolInfo[NoInfo, Repr] with SimpleInfoB[Repr] {
  override implicit def BoolInfo: NoInfo[Boolean] = NoInfo()
}

object SimpleBool {
  implicit def apply[Repr[_]] = new SimpleBool[Repr] {}
}
