package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arrow

trait Option[Info[_], Repr[_]] extends OptionInfo[Info, Repr] with Arrow[Info, Repr] {
  def none[A](implicit ai: Info[A]): Repr[scala.Option[A]]

  def some[A](implicit ai: Info[A]): Repr[A => scala.Option[A]]

  def optionMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[scala.Option[A] => B => (A => B) => B]
}
