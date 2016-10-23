package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.SimpleArrow
import com.thoughtworks.DDF.NoInfo
import com.thoughtworks.DDF.Product.SimpleProduct

trait SimpleList[Repr[_]] extends ListInfo[NoInfo, Repr] with SimpleArrow[Repr] with SimpleProduct[Repr] {
  override implicit def listInfo[A](implicit ai: NoInfo[A]) = NoInfo()

  override def listElmInfo[A] = _ => NoInfo()
}

object SimpleList {
  implicit def apply[Repr[_]] = new SimpleList[Repr] {}
}