package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Product.ShowProd
import com.thoughtworks.DDF.{NoInfo, ShowLeaf}

trait ShowList extends List[NoInfo, ShowLeaf] with ShowArr with SimpleList[ShowLeaf] with ShowProd {
  override def cons[A](implicit ai: NoInfo[A]) = ShowLeaf("Cons")

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("listMatch")

  override def nil[A](implicit ai: NoInfo[A]) = ShowLeaf("Nil")

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("listMap")

  override def reverse[A](implicit ai: NoInfo[A]) = ShowLeaf("reverse")

  override def foldRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("foldRight")

  override def foldLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("foldLeft")

  override def listZip[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("listZip")

  override def scanLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("scanLeft")

  override def scanRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = ShowLeaf("scanRight")
}

object ShowList {
  implicit def apply = new ShowList {}
}