package com.thoughtworks.DDF.Except

trait ExceptRet[Info[_], Repr[_]] extends ExceptBase[Info, Repr] {
  def exceptRet[A: Info, B: Info]: Repr[B => Except[A, B]] = right[A, B]

  final def exceptRet_[A: Info, B: Info](b: Repr[B]): Repr[Except[A, B]] = app(exceptRet[A, B])(b)
}
