package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowSum extends SumRepr[NoInfo, Show] with ShowArrow with SimpleSum[Show] {
  override def left[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("left")

  override def right[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("right")

  override def sumMatch[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]) = Show("sumMatch")

  override def sumAssocLR[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("sumAssocLR")

  override def sumAssocRL[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("sumAssocRL")

  override def sumComm[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("sumComm")
}

object ShowSum {
  implicit def apply = new ShowSum {}
}