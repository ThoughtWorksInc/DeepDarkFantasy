package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arr.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowComb extends Comb[NoInfo, Show] with ShowArr {
  override def S[A, B, C](implicit at: NoInfo[A], bt: NoInfo[B], ct: NoInfo[C]) = Show("S")

  override def K[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("K")

  override def I[A](implicit at: NoInfo[A]) = Show("I")

  override def Y[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("Y")

  override def B[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("B")

  override def W[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("W")

  override def C[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("C")
}

object ShowComb {
  implicit def apply = new ShowComb {}
}