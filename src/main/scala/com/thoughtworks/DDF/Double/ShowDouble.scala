package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowDouble extends DoubleRepr[NoInfo, Show] with ShowArrow with SimpleDouble[Show] {
  override def LitD = d => Show(d.toString)

  override def PlusD = Show("+")

  override def MultD = Show("*")
}

object ShowDouble {
  implicit def apply = new ShowDouble {}
}