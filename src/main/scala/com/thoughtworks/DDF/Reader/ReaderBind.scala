package com.thoughtworks.DDF.Reader

trait ReaderBind[Info[_], Repr[_]] extends ReaderBase[Info, Repr] {
  def readerBind[E, A, B](implicit ei: Info[E], ai: Info[A], bi: Info[B]):
  Repr[Reader[E, A] => (A => Reader[E, B]) => Reader[E, B]]

  final def readerBind_[E, A, B](m: Repr[Reader[E, A]])(implicit bi: Info[B]):
  Repr[(A => Reader[E, B]) => Reader[E, B]] =
    app(readerBind[E, A, B](readerInfoE(reprInfo(m)), readerInfoT(reprInfo(m)), bi))(m)

  final def readerBind__[E, A, B]: Repr[Reader[E, A]] => Repr[A => Reader[E, B]] => Repr[Reader[E, B]] = m => f =>
    app(readerBind_[E, A, B](m)(readerInfoT(rngInfo(reprInfo(f)))))(f)

  final def readerBind___[E, A, B]: Repr[Reader[E, A]] => Repr[A => Reader[E, B]] => Repr[E] => Repr[B] = m => f =>
    app(readerBind__[E, A, B](m)(f))
}
