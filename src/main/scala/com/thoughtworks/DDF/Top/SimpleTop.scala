package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.NoInfo

trait SimpleTop[Repr[_]] extends TopInfo[NoInfo, Repr] with SimpleInfoBase[Repr] {
  override implicit def topInfo = NoInfo()
}

object SimpleTop {
  implicit def apply[Repr[_]] = new SimpleTop[Repr] {}
}