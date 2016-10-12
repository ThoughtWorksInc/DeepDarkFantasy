package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.NoInfo

trait SimpleUnit[Repr[_]] extends UnitInfo[NoInfo, Repr] with SimpleInfoBase[Repr] {
  override implicit def unitInfo = NoInfo()
}

object SimpleUnit {
  implicit def apply[Repr[_]] = new SimpleUnit[Repr] {}
}