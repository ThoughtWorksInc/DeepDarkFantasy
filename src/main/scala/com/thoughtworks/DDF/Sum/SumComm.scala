package com.thoughtworks.DDF.Sum

trait SumComm[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumComm[A, B](implicit ai: Info[A], bi: Info[B]): Repr[Either[A, B] => Either[B, A]]

  final def sumComm_[A, B]: Repr[Either[A, B]] => Repr[Either[B, A]] = e =>
    app(sumComm(sumLeftInfo(reprInfo(e)), sumRightInfo(reprInfo(e))))(e)
}
