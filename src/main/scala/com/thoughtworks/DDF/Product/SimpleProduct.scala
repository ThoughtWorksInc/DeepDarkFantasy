package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.NoInfo

trait SimpleProduct[Repr[_]] extends ProductRepr[NoInfo, Repr] {
  override implicit def productInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def productZerothInfo[A, B] = _ => NoInfo()

  override def productFirstInfo[A, B] = _ => NoInfo()
}
