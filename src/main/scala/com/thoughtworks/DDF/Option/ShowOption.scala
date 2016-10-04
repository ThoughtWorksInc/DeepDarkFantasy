package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}



trait ShowOption extends OptionRepr[NoInfo, Show] with ShowArrow with SimpleOption[Show] {
  override def none[A](implicit ai: NoInfo[A]) = Show("none")

  override def some[A](implicit ai: NoInfo[A]) = Show("some")

  override def optionMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("optionMatch")
}
