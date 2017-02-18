package com.thoughtworks.DDF.Sum

trait SumComm extends SumMin {
  def sumComm[A <: Type: Kind, B <: Type: Kind]: Sum[A, B] ~>: Sum[B, A]

  final def sumComm_[A <: Type: Kind, B <: Type: Kind](s: Sum[A, B]) = app(sumComm[A, B])(s)
}
