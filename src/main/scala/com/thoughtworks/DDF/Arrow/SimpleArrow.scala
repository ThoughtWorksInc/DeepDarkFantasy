package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.{NoInfo, Show}

trait SimpleArrow[Repr[_]] extends ArrowInfo[NoInfo, Repr] with SimpleInfoBase[Repr] {
  override def arrowDomainInfo[A, B] = _ => NoInfo()

  override implicit def arrowInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def arrowRangeInfo[A, B] = _ => NoInfo()
}

object SimpleArrow {
  implicit def apply[Repr[_]]: SimpleArrow[Repr] = new SimpleArrow[Repr] {}
}