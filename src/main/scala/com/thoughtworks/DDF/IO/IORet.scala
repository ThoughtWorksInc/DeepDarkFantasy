package com.thoughtworks.DDF.IO

trait IORet[Info[_], Repr[_]] extends IOBase[Info, Repr] {
  def IORet[A](implicit ai: Info[A]): Repr[A => IO[A]]

  final def IORet_[A]: Repr[A] => Repr[IO[A]] = a => app(IORet(reprInfo(a)))(a)
}
