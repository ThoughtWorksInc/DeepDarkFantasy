package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.NoInfo

trait SimpleInfoBase[Repr[_]] extends InfoBase[NoInfo, Repr] {
  override def reprInfo[A] = _ => NoInfo()
}
