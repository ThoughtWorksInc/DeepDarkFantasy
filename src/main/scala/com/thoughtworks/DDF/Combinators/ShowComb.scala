package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowComb extends Comb[NoInfo, Show] with ShowArrow {
  override def S[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("S")

  override def K[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("K")

  override def I[A](implicit ai: NoInfo[A]) = Show("I")

  override def Y[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("Y")

  override def B[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("B")

  override def W[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("W")

  override def C[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("C")

  override def Let[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("Let")

  override def App[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("App")
}

object ShowComb {
  implicit def apply = new ShowComb {}
}