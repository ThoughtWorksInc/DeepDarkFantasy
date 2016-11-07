package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowStream extends Stream[NoInfo, Show] with SimpleStream[Show] with ShowTop with ShowArr {
  override def streamNil[A](implicit ai: NoInfo[A]) = Show("streamNil")

  override def streamCons[A](implicit ai: NoInfo[A]) = Show("streamCons")

  override def streamMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("streamMatch")
}

object ShowStream extends ShowStream