package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.NoInfo

trait SimpleTop[Repr[_]] extends TopInfo[NoInfo, Repr] {
  override implicit def topInfo = NoInfo()
}

object SimpleTop {
  implicit def apply[Repr[_]] = new SimpleTop[Repr] {}
}