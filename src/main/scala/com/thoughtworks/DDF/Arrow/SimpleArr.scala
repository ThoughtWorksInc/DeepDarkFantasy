package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.NoInfo

trait SimpleArr[Repr[_]] extends ArrInfo[NoInfo, Repr] {
  override implicit def aInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()
}

object SimpleArr {
  implicit def apply[Repr[_]]: SimpleArr[Repr] = new SimpleArr[Repr] {}
}