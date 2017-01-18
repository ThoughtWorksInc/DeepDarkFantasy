package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleProd[Repr[_]] extends ProdInfo[NoInfo, Repr] with SimpleArr[Repr] {
  override implicit def prodInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def prodZroInfo[A, B] = _ => NoInfo()

  override def prodFstInfo[A, B] = _ => NoInfo()
}

object SimpleProd {
  implicit def apply[Repr[_]] = new SimpleProd[Repr] {}
}