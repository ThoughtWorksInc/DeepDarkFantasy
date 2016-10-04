package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.NoInfo

trait SimpleOption[Repr[_]] extends OptionRepr[NoInfo, Repr] {
  override implicit def optionInfo[A](implicit ai: NoInfo[A]) = NoInfo()

  override def optionElmInfo[A] = _ => NoInfo()
}
