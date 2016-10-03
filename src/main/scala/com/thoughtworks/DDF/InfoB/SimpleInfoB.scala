package com.thoughtworks.DDF.InfoB

import com.thoughtworks.DDF.NoInfo

trait SimpleInfoB[Repr[_]] extends InfoB[NoInfo, Repr] {
  override def ReprInfo[A] = _ => NoInfo()
}
