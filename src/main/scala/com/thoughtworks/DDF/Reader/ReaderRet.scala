package com.thoughtworks.DDF.Reader

trait ReaderRet[Info[_], Repr[_]] extends ReaderBase[Info, Repr] {
  def readerRet[E: Info, A: Info]: Repr[A => Reader[E, A]]

  final def readerRet_[E: Info, A: Info](a: Repr[A]): Repr[Reader[E, A]] =
    app(readerRet[E, A])(a)

  final def readerRet__[E: Info, A: Info](a: Repr[A])(e: Repr[E]): Repr[A] = app(readerRet_[E, A](a))(e)
}
