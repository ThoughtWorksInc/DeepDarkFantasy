package com.thoughtworks.DDF.Sum

trait SumAssocRL extends SumMin {
  def sumAssocRL[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: Sum[A, Sum[B, C]] ~>: Sum[Sum[A, B], C]

  def sumAssocRL_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](s: Sum[A, Sum[B, C]]) = app(sumAssocRL[A, B, C])(s)
}
