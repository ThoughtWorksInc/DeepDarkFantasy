package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowProduct extends ProductRepr[NoInfo, Show] with ShowArrow with SimpleProduct[Show] {
  override def mkProduct[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("mkPair")

  override def second[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("snd")

  override def first[A, B](implicit at: NoInfo[A], bt: NoInfo[B]) = Show("fst")

  override def curry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("curry")

  override def uncurry[A, B, C](implicit ai: NoInfo[A], bi: NoInfo[B], ci: NoInfo[C]) = Show("uncurry")
}

object ShowProduct {
  implicit def apply = new ShowProduct {}
}