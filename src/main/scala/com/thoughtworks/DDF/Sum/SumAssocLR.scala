package com.thoughtworks.DDF.Sum

trait SumAssocLR extends SumMin {
  def sumAssocLR[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: Sum[Sum[A, B], C] ~>: Sum[A, Sum[B, C]]

  final def sumAssocLR_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](s: Sum[Sum[A, B], C]) =
    app(sumAssocLR[A, B, C])(s)
}
