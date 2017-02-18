package com.thoughtworks.DDF.Product

trait >< extends ProdMin {
  def ><[A, B, C, D](implicit ai: Info[A], bi: Info[B], ci: Info[C], di: Info[D]):
  Repr[(A => C) => (B => D) => (((A, B)) => (C, D))]

  final def `>_<`[A, B, C, D](ab: Repr[A => C])(implicit bi: Info[B], di: Info[D]):
  Repr[(B => D) => (((A, B)) => (C, D))] = app(><(domInfo(reprInfo(ab)), bi, rngInfo(reprInfo(ab)), di))(ab)

  def `>__<`[A, B, C, D]: Repr[A => C] => Repr[B => D] => Repr[((A, B)) => (C, D)] = ab => cd =>
    app(`>_<`(ab)(domInfo(reprInfo(cd)), rngInfo(reprInfo(cd))))(cd)

  def `>___<`[A, B, C, D]: Repr[A => C] => Repr[B => D] => Repr[(A, B)] => Repr[(C, D)] = ab => cd =>
    app(`>__<`(ab)(cd))
}
