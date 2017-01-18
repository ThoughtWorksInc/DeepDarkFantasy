package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Arrow.ArrInfo

import scalaz.effect

trait IOInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  final type IO[A] = effect.IO[A]

  implicit def IOInfo[A](implicit ai: Info[A]): Info[IO[A]]

  def IOElmInfo[A]: Info[IO[A]] => Info[A]
}
