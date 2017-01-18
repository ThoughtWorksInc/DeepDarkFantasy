package com.thoughtworks.DDF.Reader

trait ReaderBind[Info[_], Repr[_]] extends ReaderBase[Info, Repr] {
  def readerBind[E: Info, A: Info, B: Info]: Repr[Reader[E, A] => (A => Reader[E, B]) => Reader[E, B]]

  final def readerBind_[E: Info, A: Info, B: Info](m: Repr[Reader[E, A]]): Repr[(A => Reader[E, B]) => Reader[E, B]] =
    app(readerBind[E, A, B])(m)

  final def readerBind__[E: Info, A: Info, B: Info](m: Repr[Reader[E, A]])(f: Repr[A => Reader[E, B]]):
  Repr[Reader[E, B]] = app(readerBind_[E, A, B](m))(f)

  final def readerBind___[E: Info, A: Info, B: Info](m: Repr[Reader[E, A]])(f: Repr[A => Reader[E, B]])(e: Repr[E]):
  Repr[B] = app(readerBind__[E, A, B](m)(f))(e)
}
