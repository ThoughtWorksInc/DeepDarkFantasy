package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.Comb

trait NextBase[Info[_], Repr[_], Arg] {
  type repr[X] = Either[Repr[X], Repr[Arg => X]]

  implicit def comb: Comb[Info, Repr]

  implicit def argi: Info[Arg]

  def collapse[A]: repr[A] => Repr[Arg => A] = {
    case Left(x) => comb.K_(x)(argi)
    case Right(x) => x
  }

  val in: repr[Arg] = Right(comb.I(argi))

  def rconv[A]: Repr[A] => repr[A] = x => Left(x)
}
