package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.{ProductInfo, ProductRepr}



trait ListRepr[Info[_], Repr[_]] extends ProductRepr[Info, Repr] with ListBasic[Info, Repr] {
  def listMap[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => List[A] => List[B]]

  def reverse[A](implicit ai: Info[A]): Repr[List[A] => List[A]]

  def foldRight[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => B) => B => List[A] => B]

  def foldLeft[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => A) => A => List[B] => A]

  def listZip[A, B](implicit ai: Info[A], bi: Info[B]): Repr[List[A] => List[B] => List[(A, B)]]

  def scanLeft[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(B => A => B) => B => List[A] => List[B]]

  def scanRight[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => B) => B => List[A] => List[B]]
}