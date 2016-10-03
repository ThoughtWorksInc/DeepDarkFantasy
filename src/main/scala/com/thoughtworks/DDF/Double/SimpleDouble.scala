package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.NoInfo

trait SimpleDouble[Repr[_]] extends DoubleRepr[NoInfo, Repr] {
  override implicit def DoubleInfo = NoInfo()
}
