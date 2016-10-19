package com.thoughtworks.DDF.List

trait ListMin[Info[_], Repr[_]] extends ListInfo[Info, Repr] {
  def nil[A](implicit ai: Info[A]): Repr[List[A]]

  def cons[A](implicit ai: Info[A]): Repr[A => List[A] => List[A]]

  def listMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[List[A] => B => (A => List[A] => B) => B]
}
