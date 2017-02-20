package com.thoughtworks.DDF.Reader

trait ReaderRet extends ReaderBase {
  def readerRet[E <: Type: Kind, A <: Type: Kind]: A ~>: Reader[E, A]

  final def readerRet_[E <: Type: Kind, A <: Type: Kind](a: A): Reader[E, A] = app(readerRet[E, A])(a)

  final def readerRet__[E <: Type: Kind, A <: Type: Kind](a: A)(e: E) = app(readerRet_[E, A](a))(e)
}
