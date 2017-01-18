package com.thoughtworks.DDF.Product

trait ><[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  def ><[A: Info, B: Info, C: Info, D: Info]:
  Repr[(A => C) => (B => D) => (((A, B)) => (C, D))]

  final def `>_<`[A: Info, B: Info, C: Info, D: Info](ac: Repr[A => C]):
  Repr[(B => D) => (((A, B)) => (C, D))] = app(><[A, B, C, D])(ac)

  def `>__<`[A: Info, B: Info, C: Info, D: Info](ac: Repr[A => C])(bd: Repr[B => D]): Repr[((A, B)) => (C, D)] =
    app(`>_<`[A, B, C, D](ac))(bd)

  def `>___<`[A: Info, B: Info, C: Info, D: Info](ac: Repr[A => C])(bd: Repr[B => D])(ab: Repr[(A, B)]): Repr[(C, D)] =
    app(`>__<`(ac)(bd))(ab)
}
