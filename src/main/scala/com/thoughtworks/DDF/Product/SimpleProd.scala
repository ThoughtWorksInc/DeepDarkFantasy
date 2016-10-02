package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.NoInfo

trait SimpleProd[Repr[_]] extends ProdLang[NoInfo, Repr] {
  override implicit def ProdInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def ProdFstInfo[A, B] = _ => NoInfo()

  override def ProdSndInfo[A, B] = _ => NoInfo()
}
