package com.thoughtworks.DDF.Product

trait >< extends ProdMin {
  def ><[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind, D <: Type: Kind]:
  (A ~>: C) ~>: (B ~>: D) ~>: Prod[A, B] ~>: Prod[C, D]

  final def `>_<`[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind, D <: Type: Kind](ac: A ~>: C) =
    app(><[A, B, C, D])(ac)

  def `>__<`[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind, D <: Type: Kind](ac: A ~>: C)(bd: B ~>: D) =
    app(`>_<`[A, B, C, D](ac))(bd)

  def `>___<`[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind, D <: Type: Kind]
  (ac: A ~>: C)(bd: B ~>: D)(p: Prod[A, B]) = app(`>__<`(ac)(bd))(p)
}
