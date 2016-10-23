package com.thoughtworks.DDF.Sum

trait Sum[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumComm[A, B](implicit ai: Info[A], bi: Info[B]): Repr[Either[A, B] => Either[B, A]]

  def sumAssocLR[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Either[Either[A, B], C] => Either[A, Either[B, C]]]

  def sumAssocRL[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]):
  Repr[Either[A, Either[B, C]] => Either[Either[A, B], C]]
}
