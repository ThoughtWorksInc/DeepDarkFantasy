package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.NoInfo

trait SimpleUnit[Repr[_]] extends UnitLang[NoInfo, Repr] {
  override implicit def UnitInfo = NoInfo()
}
