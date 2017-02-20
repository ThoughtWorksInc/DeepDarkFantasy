package com.thoughtworks.DDF.Reader

trait ReaderBind extends ReaderBase {
  def readerBind[E <: Type: Kind, A <: Type: Kind, B <: Type: Kind]:
  Reader[E, A] ~>: (A ~>: Reader[E, B]) ~>: Reader[E, B]

  final def readerBind_[E <: Type: Kind, A <: Type: Kind, B <: Type: Kind](m: Reader[E, A]) =
    app(readerBind[E, A, B])(m)

  final def readerBind__[E <: Type: Kind, A <: Type: Kind, B <: Type: Kind](m: Reader[E, A])(f: A ~>: Reader[E, B]) =
    app(readerBind_[E, A, B](m))(f)

  final def readerBind___[E <: Type: Kind, A <: Type: Kind, B <: Type: Kind]
  (m: Reader[E, A])(f: A ~>: Reader[E, B])(e: E) =
    app(readerBind__[E, A, B](m)(f))(e)
}
