package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arr.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowD extends DLang[NoInfo, Show] with ShowArr with SimpleD[Show] {
  override def LitD = d => Show(d.toString)

  override def PlusD = Show("+")

  override def MultD = Show("*")
}

object ShowD {
  implicit def apply = new ShowD {}
}