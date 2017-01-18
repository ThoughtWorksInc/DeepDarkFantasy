package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Arrow.ArrInfo
import com.thoughtworks.DDF.Top.TopInfo

import scalaz.effect

trait IOInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] with TopInfo[Info, Repr] {
  final type IO[A] = effect.IO[A]

  implicit def IOInfo[A](implicit ai: Info[A]): Info[IO[A]]
}
