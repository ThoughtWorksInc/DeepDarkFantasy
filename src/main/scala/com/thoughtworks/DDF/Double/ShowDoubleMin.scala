package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowDoubleMin extends
  DoubleMin[NoInfo, Lambda[X => Show]] with ShowArr with SimpleDouble[Lambda[X => Show]] with ShowBool {
  override def litD = d => Show(d.toString)

  override def plusD = Show("+")

  override def multD = Show("*")

  override def recipD = Show("recipD")

  override def expD = Show("expD")

  override def sigD = Show("sigmoidD")

  override def ltD = Show("<")
}

object ShowDoubleMin extends ShowDoubleMin