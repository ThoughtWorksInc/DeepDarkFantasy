package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowArr extends Arr[NoInfo, Show] with SimpleArr[Show] {
  override def app[A, B] = f => x => Show("(" + f.s + " " + x.s + ")")
}

object ShowArr {
  implicit def apply = new ShowArr {}
}