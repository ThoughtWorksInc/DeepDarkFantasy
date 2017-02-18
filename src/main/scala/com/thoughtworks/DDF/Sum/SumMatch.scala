package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait SumMatch extends Arr with SumType {
  def sumMatch[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: Sum[A, B] ~>: (A ~>: C) ~>: (B ~>: C) ~>: C

  final def sumMatch_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](e: Sum[A, B]) = app(sumMatch[A, B, C])(e)

  final def sumMatch__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](e: Sum[A, B])(ac: A ~>: C) =
    app(sumMatch_[A, B, C](e))(ac)

  final def sumMatch___[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](e: Sum[A, B])(ac: A ~>: C)(bc: B ~>: C) =
    app(sumMatch__(e)(ac))(bc)
}
