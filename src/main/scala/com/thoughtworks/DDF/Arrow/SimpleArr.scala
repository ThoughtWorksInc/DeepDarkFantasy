package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.SimpleInfoBase
import com.thoughtworks.DDF.{NoInfo, Show}

trait SimpleArr[Repr[_]] extends ArrInfo[NoInfo, Repr] with SimpleInfoBase[Repr] {
  override def domInfo[A, B] = _ => NoInfo()

  override implicit def aInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def rngInfo[A, B] = _ => NoInfo()
}

object SimpleArr {
  implicit def apply[Repr[_]]: SimpleArr[Repr] = new SimpleArr[Repr] {}
}