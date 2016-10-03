package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait OptionRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def none[A](implicit ai: Info[A]): Repr[Option[A]]

  def some[A](implicit ai: Info[A]): Repr[A => Option[A]]

  def optionMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[B => (A => B) => Option[A] => B]
}
