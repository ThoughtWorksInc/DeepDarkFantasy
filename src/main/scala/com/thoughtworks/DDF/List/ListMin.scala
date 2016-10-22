package com.thoughtworks.DDF.List

trait ListMin[Info[_], Repr[_]] extends ListInfo[Info, Repr] {
  def nil[A](implicit ai: Info[A]): Repr[scala.List[A]]

  def cons[A](implicit ai: Info[A]): Repr[A => scala.List[A] => scala.List[A]]

  def listMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[scala.List[A] => B => (A => scala.List[A] => B) => B]
}
