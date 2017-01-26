package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowOption extends Option[NoInfo, ShowLeaf] with ShowArr with SimpleOption[ShowLeaf] {
  override def none[A](implicit ai: NoInfo[A]) = ShowLeaf("none")

  override def some[A](implicit ai: NoInfo[A]) = ShowLeaf("some")

  override def optionMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("optionMatch")
}

object ShowOption {
  implicit def apply = new ShowOption {}
}