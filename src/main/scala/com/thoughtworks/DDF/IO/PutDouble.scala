package com.thoughtworks.DDF.IO

trait PutDouble extends IOBase {
  def putDouble: Double ~>: IO[Top]

  final lazy val putDouble_ = app(putDouble)_
}
