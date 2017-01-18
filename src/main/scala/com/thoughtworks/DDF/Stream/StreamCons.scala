package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Top.Top

trait StreamCons[Info[_], Repr[_]] extends StreamInfo[Info, Repr] with Top[Info, Repr] with Arr[Info, Repr] {
  def streamCons[A](implicit ai: Info[A]): Repr[A => (Unit => scala.Stream[A]) => scala.Stream[A]]

  final def streamCons_[A]: Repr[A] => Repr[(Unit => scala.Stream[A]) => scala.Stream[A]] = a =>
    app(streamCons(reprInfo(a)))(a)

  final def streamCons__[A]: Repr[A] => Repr[Unit => scala.Stream[A]] => Repr[scala.Stream[A]] =
    a => app(streamCons_(a))
}
