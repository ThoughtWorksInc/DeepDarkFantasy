package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowComb extends Comb[NoInfo, ShowLeaf] with ShowArr {
  override def S[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("S")

  override def K[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("K")

  override def I[A](implicit ai: NoInfo[A]) = ShowLeaf("I")

  override def Y[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("Y")

  override def B[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("B")

  override def W[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("W")

  override def C[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = ShowLeaf("C")

  override def Let[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("Let")

  override def App[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("App")
}

object ShowComb {
  implicit def apply = new ShowComb {}
}