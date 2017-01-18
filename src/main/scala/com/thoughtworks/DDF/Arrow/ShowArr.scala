package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowArr extends Arr[NoInfo, Show] with SimpleArr[Show] {
  override def app[A: NoInfo, B: NoInfo](f: Show[A => B])(x: Show[A]) = Show("(" + f.s + " " + x.s + ")")
}

object ShowArr extends ShowArr