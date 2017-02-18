package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.LangBase.SimpleLangBase
import com.thoughtworks.DDF.NoInfo

trait SimpleArr[Repr[_]] extends Arr with SimpleLangBase {
  override def domInfo[A, B] = _ => NoInfo()

  override implicit def aInfo[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = NoInfo()

  override def rngInfo[A, B] = _ => NoInfo()
}

object SimpleArr {
  implicit def apply[Repr[_]]: SimpleArr[Repr] = new SimpleArr[Repr] {}
}