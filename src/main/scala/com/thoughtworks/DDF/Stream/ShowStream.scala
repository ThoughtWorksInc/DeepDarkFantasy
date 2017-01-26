package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Top.ShowTop
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowStream extends Stream[NoInfo, ShowLeaf] with SimpleStream[ShowLeaf] with ShowTop with ShowArr {
  override def streamNil[A](implicit ai: NoInfo[A]) = ShowLeaf("streamNil")

  override def streamCons[A](implicit ai: NoInfo[A]) = ShowLeaf("streamCons")

  override def streamMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("streamMatch")
}

object ShowStream extends ShowStream