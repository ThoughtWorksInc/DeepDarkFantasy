package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowSum extends Sum[NoInfo, ShowLeaf] with ShowArr with SimpleSum[ShowLeaf] {
  override def left[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = ShowLeaf("left")

  override def right[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = ShowLeaf("right")

  override def sumMatch[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]) = ShowLeaf("sumMatch")

  override def sumAssocLR[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("sumAssocLR")

  override def sumAssocRL[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("sumAssocRL")

  override def sumComm[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("sumComm")
}

object ShowSum {
  implicit def apply = new ShowSum {}
}