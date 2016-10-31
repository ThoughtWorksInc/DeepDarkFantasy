package com.thoughtworks.DDF.Except

trait ExceptRet[Info[_], Repr[_]] extends ExceptBase[Info, Repr] {
  def exceptRet[A, B](implicit ai: Info[A], bi: Info[B]): Repr[B => Except[A, B]] = right[A, B]

  final def exceptRet_[A, B](b: Repr[B])(implicit ai: Info[A]): Repr[Except[A, B]] = app(exceptRet(ai, reprInfo(b)))(b)
}
