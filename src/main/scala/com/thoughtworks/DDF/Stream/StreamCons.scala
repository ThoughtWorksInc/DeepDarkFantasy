package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Top.Top

trait StreamCons[Info[_], Repr[_]] extends StreamInfo[Info, Repr] with Top[Info, Repr] with Arr[Info, Repr] {
  def streamCons[A: Info]: Repr[A => (Unit => scala.Stream[A]) => scala.Stream[A]]

  final def streamCons_[A: Info](a: Repr[A]): Repr[(Unit => scala.Stream[A]) => scala.Stream[A]] =
    app(streamCons[A])(a)

  final def streamCons__[A: Info](a: Repr[A])(t: Repr[Unit => scala.Stream[A]]): Repr[scala.Stream[A]] =
    app(streamCons_(a))(t)
}
