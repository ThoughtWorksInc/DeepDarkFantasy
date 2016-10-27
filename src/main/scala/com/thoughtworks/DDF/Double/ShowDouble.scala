package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowDouble extends Double[NoInfo, Show] with ShowArrow with SimpleDouble[Show] with ShowBool {
  override def litD = d => Show(d.toString)

  override def plusD = Show("+")

  override def multD = Show("*")

  override def divD = Show("/")

  override def expD = Show("expD")

  override def sigD = Show("sigmoidD")

  override def ltD = Show("<")
}

object ShowDouble {
  implicit def apply = new ShowDouble {}
}