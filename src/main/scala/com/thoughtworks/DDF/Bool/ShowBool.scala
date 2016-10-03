package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowBool extends BoolRepr[NoInfo, Show] with SimpleBool[Show] with ShowArrow {
  override def litB = b => Show(b.toString)

  override def ite[A](implicit ai: NoInfo[A]) = Show("ite")
}

object ShowBool {
  implicit def apply = new ShowBool {}
}