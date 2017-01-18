package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arr

trait OptionMatch[Info[_], Repr[_]] extends OptionInfo[Info, Repr] with Arr[Info, Repr] {
  def optionMatch[A: Info, B: Info]: Repr[scala.Option[A] => B => (A => B) => B]

  final def optionMatch_[A: Info, B: Info](oa: Repr[scala.Option[A]]): Repr[B => (A => B) => B] =
    app(optionMatch[A, B])(oa)

  final def optionMatch__[A: Info, B: Info](oa: Repr[scala.Option[A]])(b: Repr[B]): Repr[(A => B) => B] =
    app(optionMatch_[A, B](oa))(b)

  final def optionMatch___[A: Info, B: Info](oa: Repr[scala.Option[A]])(b: Repr[B])(f: Repr[A => B]): Repr[B] =
    app(optionMatch__(oa)(b))(f)
}
