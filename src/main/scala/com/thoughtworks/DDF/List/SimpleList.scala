package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Unit.SimpleUnit

trait SimpleList[Repr[_]] extends ListRepr[NoInfo, Repr] with SimpleUnit[Repr] {
  override implicit def ListInfo[A](implicit ai: NoInfo[A]) = NoInfo()

  override def ListElmInfo[A](implicit lai: NoInfo[List[A]]) = NoInfo()
}
