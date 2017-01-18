package com.thoughtworks.DDF.IO

trait IOBind[Info[_], Repr[_]] extends IOBase[Info, Repr] {
  def IOBind[A: Info, B: Info]: Repr[IO[A] => (A => IO[B]) => IO[B]]

  final def IOBind_[A: Info, B: Info](m: Repr[IO[A]]): Repr[(A => IO[B]) => IO[B]] = app(IOBind[A, B])(m)

  final def IOBind__[A: Info, B: Info](m: Repr[IO[A]])(f: Repr[A => IO[B]]): Repr[IO[B]] = app(IOBind_[A, B](m))(f)
}
