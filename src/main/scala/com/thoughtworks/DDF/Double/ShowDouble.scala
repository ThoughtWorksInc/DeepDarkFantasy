package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowDouble extends DoubleRepr[NoInfo, Show] with ShowArrow with SimpleDouble[Show] {
  override def litD = d => Show(d.toString)

  override def plusD = Show("+")

  override def multD = Show("*")
}

object ShowDouble {
  implicit def apply = new ShowDouble {}
}