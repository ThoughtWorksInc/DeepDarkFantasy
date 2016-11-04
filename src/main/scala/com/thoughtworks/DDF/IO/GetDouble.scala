package com.thoughtworks.DDF.IO

trait GetDouble[Info[_], Repr[_]] extends IOBase[Info, Repr] {
  def getDouble: Repr[IO[Double]]
}
