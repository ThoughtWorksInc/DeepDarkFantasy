package com.thoughtworks.DDF.Sum

trait SumAssocLR[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumAssocLR[A: Info, B: Info, C: Info]:
  Repr[Either[Either[A, B], C] => Either[A, Either[B, C]]]

  final def sumAssocLR_[A: Info, B: Info, C: Info](x: Repr[Either[Either[A, B], C]]): Repr[Either[A, Either[B, C]]] =
    app(sumAssocLR[A, B, C])(x)
}
