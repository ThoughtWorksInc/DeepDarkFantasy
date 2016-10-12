package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.SimpleArrow
import com.thoughtworks.DDF.NoInfo

trait SimpleSum[Repr[_]] extends SumInfo[NoInfo, Repr] with SimpleArrow[Repr] {
  override implicit def sumInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def sumLeftInfo[A, B]  = _ => NoInfo()

  override def sumRightInfo[A, B] = _ => NoInfo()
}

object SimpleSum {
  implicit def apply[Repr[_]] = new SimpleSum[Repr] {}
}