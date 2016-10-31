package com.thoughtworks.DDF.Except

trait ExceptBind[Info[_], Repr[_]] extends ExceptBase[Info, Repr] {
  def exceptBind[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Except[A, B] => (B => Except[A, C]) => Except[A, C]]

  final def exceptBind_[A, B, C](e: Repr[Except[A, B]])(implicit ci: Info[C]):
  Repr[(B => Except[A, C]) => Except[A, C]] =
    app(exceptBind(sumLeftInfo(reprInfo(e)), sumRightInfo(reprInfo(e)), ci))(e)

  final def exceptBind__[A, B, C]: Repr[Except[A, B]] => Repr[B => Except[A, C]] => Repr[Except[A, C]] = e => f =>
    app(exceptBind_(e)(sumRightInfo(rngInfo(reprInfo(f)))))(f)
}
