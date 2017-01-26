package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ShowArr
import com.thoughtworks.DDF.Product.ShowProd
import com.thoughtworks.DDF.{NoInfo, Show}

trait ShowList extends List[NoInfo, Lambda[X => Show]] with ShowArr with SimpleList[Lambda[X => Show]] with ShowProd {
  override def cons[A](implicit ai: NoInfo[A]) = Show("Cons")

  override def listMatch[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMatch")

  override def nil[A](implicit ai: NoInfo[A]) = Show("Nil")

  override def listMap[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listMap")

  override def reverse[A](implicit ai: NoInfo[A]) = Show("reverse")

  override def foldRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("foldRight")

  override def foldLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("foldLeft")

  override def listZip[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("listZip")

  override def scanLeft[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("scanLeft")

  override def scanRight[A, B](implicit ai: NoInfo[A], bi: NoInfo[B]) = Show("scanRight")
}

object ShowList extends ShowList