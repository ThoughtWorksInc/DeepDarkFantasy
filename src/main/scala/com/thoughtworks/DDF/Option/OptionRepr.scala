package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait OptionRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  implicit def optionInfo[A](implicit ai: Info[A]): Info[Option[A]]

  def optionElmInfo[A]: Info[Option[A]] => Info[A]

  def none[A](implicit ai: Info[A]): Repr[Option[A]]

  def some[A](implicit ai: Info[A]): Repr[A => Option[A]]

  def optionMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[Option[A] => B => (A => B) => B]
}
