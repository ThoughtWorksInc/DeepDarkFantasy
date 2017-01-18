package com.thoughtworks.DDF.IO

trait IORet[Info[_], Repr[_]] extends IOBase[Info, Repr] {
  def IORet[A](implicit ai: Info[A]): Repr[A => IO[A]]

  final def IORet_[A: Info](a: Repr[A]): Repr[IO[A]] = app(IORet[A])(a)
}
