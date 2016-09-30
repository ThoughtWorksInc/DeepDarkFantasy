package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowList extends ListLang[NoInfo, Show] {
  override def Cons[A](implicit ai: NoInfo[A]) = Show("Cons")

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMatch")

  override def Nil[A](implicit ai: NoInfo[A]) = Show("Nil")

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMap")
}
