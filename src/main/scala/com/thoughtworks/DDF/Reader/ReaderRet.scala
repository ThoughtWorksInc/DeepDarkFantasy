package com.thoughtworks.DDF.Reader

trait ReaderRet[Info[_], Repr[_]] extends ReaderBase[Info, Repr] {
  def readerRet[E, A](implicit ei: Info[E], ai: Info[A]): Repr[A => Reader[E, A]]

  final def readerRet_[E, A](a: Repr[A])(implicit ei: Info[E]): Repr[Reader[E, A]] =
    app(readerRet[E, A](ei, reprInfo(a)))(a)

  final def readerRet__[E, A]: Repr[A] => Repr[E] => Repr[A] = a => e => app(readerRet_[E, A](a)(reprInfo(e)))(e)
}
