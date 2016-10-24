package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowArrow extends Arrow[NoInfo, Show] with SimpleArrow[Show] {
  override def app[A, B] = f => x => Show("(" + f.s + " " + x.s + ")")
}

object ShowArrow {
  implicit def apply = new ShowArrow {}
}