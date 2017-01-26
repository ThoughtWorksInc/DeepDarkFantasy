package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Bool.ShowBool
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowDoubleMin extends DoubleMin[NoInfo, ShowLeaf] with ShowArr with SimpleDouble[ShowLeaf] with ShowBool {
  override def litD = d => ShowLeaf(d.toString)

  override def plusD = ShowLeaf("+")

  override def multD = ShowLeaf("*")

  override def recipD = ShowLeaf("recipD")

  override def expD = ShowLeaf("expD")

  override def sigD = ShowLeaf("sigmoidD")

  override def ltD = ShowLeaf("<")
}

object ShowDoubleMin {
  implicit def apply = new ShowDoubleMin {}
}