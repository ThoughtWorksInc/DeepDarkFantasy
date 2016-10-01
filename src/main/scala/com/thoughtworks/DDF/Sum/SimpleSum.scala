package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.NoInfo

trait SimpleSum[Repr[_]] extends SumLang[NoInfo, Repr] {
  override def SumLeftInfo[A, B]  = _ => NoInfo()

  override def SumRightInfo[A, B] = _ => NoInfo()

  override def SumInfo[A, B] = _ => _ => NoInfo()

}
