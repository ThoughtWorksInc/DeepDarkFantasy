package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.SimpleArr
import com.thoughtworks.DDF.NoInfo

trait SimpleOption[Repr[_]] extends OptionType[NoInfo, Repr] with SimpleArr[Repr] {
  override implicit def optionInfo[A](implicit ai: NoInfo[A]) = NoInfo()

  override def optionElmInfo[A] = _ => NoInfo()
}

object SimpleOption {
  implicit def apply[Repr[_]] = new SimpleOption[Repr] {}
}