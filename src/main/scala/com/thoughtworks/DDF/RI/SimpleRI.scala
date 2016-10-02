package com.thoughtworks.DDF.RI

import com.thoughtworks.DDF.NoInfo

trait SimpleRI[Repr[_]] extends RILang[NoInfo, Repr] {
  override def ReprInfo[A] = _ => NoInfo()
}
