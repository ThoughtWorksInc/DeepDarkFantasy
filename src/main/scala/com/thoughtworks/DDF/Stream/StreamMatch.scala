package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.Arr

trait StreamMatch[Info[_], Repr[_]] extends StreamInfo[Info, Repr] with Arr[Info, Repr] {
  def streamMatch[A, B](implicit ai: Info[A], bi: Info[B]):
  Repr[scala.Stream[A] => B => (A => scala.Stream[A] => B) => B]

  def streamMatch_[A, B](sa: Repr[scala.Stream[A]])(implicit bi: Info[B]): Repr[B => (A => scala.Stream[A] => B) => B] =
    app(streamMatch(streamElmInfo(reprInfo(sa)), bi))(sa)

  final def streamMatch__[A, B]: Repr[scala.Stream[A]] => Repr[B] => Repr[(A => scala.Stream[A] => B) => B] = sa => b =>
    app(streamMatch_(sa)(reprInfo(b)))(b)

  final def streamMatch___[A, B]: Repr[scala.Stream[A]] => Repr[B] => Repr[A => scala.Stream[A] => B] => Repr[B] =
    sa => b => app(streamMatch__(sa)(b))
}
