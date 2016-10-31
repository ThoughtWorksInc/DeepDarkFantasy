package com.thoughtworks.DDF.Sum

trait SumAssocLR[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumAssocLR[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Either[Either[A, B], C] => Either[A, Either[B, C]]]

  final def sumAssocLR_[A, B, C]: Repr[Either[Either[A, B], C]] => Repr[Either[A, Either[B, C]]] = x =>
    app(sumAssocLR(
      sumLeftInfo(sumLeftInfo(reprInfo(x))),
      sumRightInfo(sumLeftInfo(reprInfo(x))),
      sumRightInfo(reprInfo(x))))(x)
}
