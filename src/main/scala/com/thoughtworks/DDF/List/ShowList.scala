package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ShowArrow
import com.thoughtworks.DDF.Unit.ShowUnit
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowList extends ListRepr[NoInfo, Show] with ShowArrow with ShowUnit with SimpleList[Show] {
  override def cons[A](implicit ai: NoInfo[A]) = Show("Cons")

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMatch")

  override def nil[A](implicit ai: NoInfo[A]) = Show("Nil")

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMap")
}

object ShowList {
  implicit def apply = new ShowList {}
}