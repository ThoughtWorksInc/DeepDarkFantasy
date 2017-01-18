package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleProd[Repr[_]] extends ProdInfo[NoInfo, Repr] with SimpleArr[Repr] {
  override implicit def prodInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()
}

object SimpleProd {
  implicit def apply[Repr[_]] = new SimpleProd[Repr] {}
}