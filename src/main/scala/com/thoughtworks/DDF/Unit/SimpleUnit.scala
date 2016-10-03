package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.NoInfo

trait SimpleUnit[Repr[_]] extends UnitRepr[NoInfo, Repr] {
  override implicit def unitInfo = NoInfo()
}
