package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.NoInfo

trait SimpleSum[Repr[_]] extends SumRepr[NoInfo, Repr] {
  override implicit def sumInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def sumLeftInfo[A, B]  = _ => NoInfo()

  override def sumRightInfo[A, B] = _ => NoInfo()
}
