package com.thoughtworks.DDF.Sum

trait SumAssocRL[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumAssocRL[A: Info, B: Info, C: Info]:
  Repr[Either[A, Either[B, C]] => Either[Either[A, B], C]]

  def sumAssocRL_[A: Info, B: Info, C: Info](x: Repr[Either[A, Either[B, C]]]): Repr[Either[Either[A, B], C]] =
    app(sumAssocRL[A, B, C])(x)
}
