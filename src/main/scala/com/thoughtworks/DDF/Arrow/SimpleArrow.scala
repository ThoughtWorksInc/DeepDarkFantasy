package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{NoInfo, Show}

trait SimpleArrow[Repr[_]] extends ArrowRepr[NoInfo, Repr] {
  override def arrowDomainInfo[A, B] = _ => NoInfo()

  override implicit def arrowInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def arrowRangeInfo[A, B] = _ => NoInfo()

  override def reprInfo[A] = _ => NoInfo()
}
