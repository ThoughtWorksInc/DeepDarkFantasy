package com.thoughtworks.DDF.IO

trait IORet extends IOBase {
  def IORet[A <: Type: Kind]: A ~>: IO[A]

  final def IORet_[A <: Type: Kind](a: A) = app(IORet[A])(a)
}
