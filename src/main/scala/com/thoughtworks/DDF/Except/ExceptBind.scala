package com.thoughtworks.DDF.Except

trait ExceptBind[Info[_], Repr[_]] extends ExceptBase[Info, Repr] {
  def exceptBind[A: Info, B: Info, C: Info]: Repr[Except[A, B] => (B => Except[A, C]) => Except[A, C]]

  final def exceptBind_[A: Info, B: Info, C: Info](e: Repr[Except[A, B]]): Repr[(B => Except[A, C]) => Except[A, C]] =
    app(exceptBind[A, B, C])(e)

  final def exceptBind__[A: Info, B: Info, C: Info](e: Repr[Except[A, B]])(f: Repr[B => Except[A, C]]):
  Repr[Except[A, C]] =
    app(exceptBind_[A, B, C](e))(f)
}
