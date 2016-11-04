package com.thoughtworks.DDF.IO

trait IOBind[Info[_], Repr[_]] extends IOBase[Info, Repr] {
  def IOBind[A, B](implicit ai: Info[A], bi: Info[B]): Repr[IO[A] => (A => IO[B]) => IO[B]]

  final def IOBind_[A, B](m: Repr[IO[A]])(implicit bi: Info[B]): Repr[(A => IO[B]) => IO[B]] =
    app(IOBind(IOElmInfo(reprInfo(m)), bi))(m)

  final def IOBind__[A, B]: Repr[IO[A]] => Repr[A => IO[B]] => Repr[IO[B]] = m => f =>
    app(IOBind_(m)(IOElmInfo(rngInfo(reprInfo(f)))))(f)
}
