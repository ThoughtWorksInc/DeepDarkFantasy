package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleSum[Repr[_]] extends SumInfo[NoInfo, Repr] with SimpleArr[Repr] {
  override implicit def sumInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()
}

object SimpleSum {
  implicit def apply[Repr[_]] = new SimpleSum[Repr] {}
}