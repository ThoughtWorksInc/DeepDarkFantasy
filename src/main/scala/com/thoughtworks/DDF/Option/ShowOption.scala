package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowOption extends Option[NoInfo, Lambda[X => Show]] with ShowArr with SimpleOption[Lambda[X => Show]] {
  override def none[A](implicit ai: NoInfo[A]) = Show("none")

  override def some[A](implicit ai: NoInfo[A]) = Show("some")

  override def optionMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("optionMatch")
}

object ShowOption extends ShowOption