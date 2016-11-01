package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Product.SimpleProd

trait SimpleList[Repr[_]] extends ListInfo[NoInfo, Repr] with SimpleArr[Repr] with SimpleProd[Repr] {
  override implicit def listInfo[A](implicit ai: NoInfo[A]) = NoInfo()

  override def listElmInfo[A] = _ => NoInfo()
}

object SimpleList {
  implicit def apply[Repr[_]] = new SimpleList[Repr] {}
}