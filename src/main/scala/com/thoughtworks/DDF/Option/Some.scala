package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.Arr

trait Some[Info[_], Repr[_]] extends OptionInfo[Info, Repr] with Arr[Info, Repr] {
  def some[A](implicit ai: Info[A]): Repr[A => scala.Option[A]]

  final def some_[A]: Repr[A] => Repr[scala.Option[A]] = a => app(some(reprInfo(a)))(a)
}
