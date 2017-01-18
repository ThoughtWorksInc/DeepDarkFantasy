package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr

trait StreamMatch[Info[_], Repr[_]] extends StreamInfo[Info, Repr] with Arr[Info, Repr] {
  def streamMatch[A: Info, B: Info]: Repr[scala.Stream[A] => B => (A => scala.Stream[A] => B) => B]

  def streamMatch_[A: Info, B: Info](sa: Repr[scala.Stream[A]]): Repr[B => (A => scala.Stream[A] => B) => B] =
    app(streamMatch[A, B])(sa)

  final def streamMatch__[A: Info, B: Info](sa: Repr[scala.Stream[A]])(b: Repr[B]):
  Repr[(A => scala.Stream[A] => B) => B] = app(streamMatch_[A, B](sa))(b)

  final def streamMatch___[A: Info, B: Info](sa: Repr[scala.Stream[A]])(b: Repr[B])(f: Repr[A => scala.Stream[A] => B]):
  Repr[B] = app(streamMatch__(sa)(b))(f)
}
