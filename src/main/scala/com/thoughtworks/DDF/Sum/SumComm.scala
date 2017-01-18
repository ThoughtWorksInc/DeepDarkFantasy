package com.thoughtworks.DDF.Sum

trait SumComm[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  def sumComm[A: Info, B: Info]: Repr[Either[A, B] => Either[B, A]]

  final def sumComm_[A: Info, B: Info](e: Repr[Either[A, B]]): Repr[Either[B, A]] = app(sumComm[A, B])(e)
}
