package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.NoInfo

trait SimpleD[Repr[_]] extends DLang[NoInfo, Repr] {
  override def DoubleInfo = NoInfo()
}
