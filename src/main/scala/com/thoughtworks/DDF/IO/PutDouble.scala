package com.thoughtworks.DDF.IO

trait PutDouble[Info[_], Repr[_]] extends IOBase[Info, Repr] {
  def putDouble: Repr[Double => IO[Unit]]

  final lazy val putDouble_ : Repr[Double] => Repr[IO[Unit]] = app(putDouble)
}
