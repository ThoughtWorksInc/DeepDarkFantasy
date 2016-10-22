package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.{ProductInfo, ProductRepr}



trait List[Info[_], Repr[_]] extends ProductRepr[Info, Repr] with ListMin[Info, Repr] {
  def listMap[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => scala.List[A] => scala.List[B]]

  def reverse[A](implicit ai: Info[A]): Repr[scala.List[A] => scala.List[A]]

  def foldRight[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => B) => B => scala.List[A] => B]

  def foldLeft[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => A) => A => scala.List[B] => A]

  def listZip[A, B](implicit ai: Info[A], bi: Info[B]): Repr[scala.List[A] => scala.List[B] => scala.List[(A, B)]]

  def scanLeft[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(B => A => B) => B => scala.List[A] => scala.List[B]]

  def scanRight[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => B) => B => scala.List[A] => scala.List[B]]
}