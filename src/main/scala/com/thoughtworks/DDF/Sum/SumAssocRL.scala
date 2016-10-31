package com.thoughtworks.DDF.Sum

trait SumAssocRL[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumAssocRL[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Either[A, Either[B, C]] => Either[Either[A, B], C]]

  def sumAssocRL_[A, B, C]: Repr[Either[A, Either[B, C]]] => Repr[Either[Either[A, B], C]] = x =>
    app(sumAssocRL(
      sumLeftInfo(reprInfo(x)),
      sumLeftInfo(sumRightInfo(reprInfo(x))),
      sumRightInfo(sumRightInfo(reprInfo(x)))))(x)
}
