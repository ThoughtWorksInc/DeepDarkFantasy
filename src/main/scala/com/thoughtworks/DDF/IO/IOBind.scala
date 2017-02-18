package com.thoughtworks.DDF.IO

trait IOBind extends IOBase {
  def IOBind[A <: Type: Kind, B <: Type: Kind]: IO[A] ~>: (A ~>: IO[B]) ~>: IO[B]

  final def IOBind_[A <: Type: Kind, B <: Type: Kind](m: IO[A]) = app(IOBind[A, B])(m)

  final def IOBind__[A <: Type: Kind, B <: Type: Kind](m: IO[A])(f: A ~>: IO[B]) = app(IOBind_[A, B](m))(f)
}
