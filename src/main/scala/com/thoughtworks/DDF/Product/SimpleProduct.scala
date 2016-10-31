package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleProduct[Repr[_]] extends ProductInfo[NoInfo, Repr] with SimpleArr[Repr] {
  override implicit def productInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def productZerothInfo[A, B] = _ => NoInfo()

  override def productFirstInfo[A, B] = _ => NoInfo()
}

object SimpleProduct {
  implicit def apply[Repr[_]] = new SimpleProduct[Repr] {}
}