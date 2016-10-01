package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.NoInfo

trait SimpleProd[Repr[_]] extends ProdLang[NoInfo, Repr] {
  override def ProdInfo[A, B] = _ => _ => NoInfo()

  override def ProdFstInfo[A, B] = _ => NoInfo()

  override def ProdSndInfo[A, B] = _ => NoInfo()
}
