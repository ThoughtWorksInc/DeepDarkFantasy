package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arr

trait Some[Info[_], Repr[_]] extends OptionInfo[Info, Repr] with Arr[Info, Repr] {
  def some[A: Info]: Repr[A => scala.Option[A]]

  final def some_[A: Info](a: Repr[A]): Repr[scala.Option[A]] = app(some)(a)
}
