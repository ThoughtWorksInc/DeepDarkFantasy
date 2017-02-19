package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Bool.EvalMBool
import com.thoughtworks.DDF.NoInfo

trait EvalMDoubleMin extends
  DoubleMin with
  SimpleDouble with
  EvalMBool {
  override def recipD: scala.Double => scala.Double = x => 1 / x

  override def ltD: scala.Double => scala.Double => Boolean = l => r => l < r

  override def plusD: scala.Double => scala.Double => scala.Double = l => r => l + r

  override def sigD: scala.Double => scala.Double = x => 1 / (1 + Math.exp(-x))

  override def multD: scala.Double => scala.Double => scala.Double = l => r => l * r

  override def litD = identity[scala.Double]

  override def expD: scala.Double => scala.Double = Math.exp
}

object EvalMDoubleMin extends EvalMDoubleMin