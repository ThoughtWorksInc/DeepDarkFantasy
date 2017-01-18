package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.Comb

trait NextBase[Info[_], Repr[_], Arg] {
  type repr[X] = (Info[X], Either[Repr[X], Repr[Arg => X]])

  implicit def comb: Comb[Info, Repr]

  def argi: Info[Arg]

  def collapse[A](r: repr[A]): Repr[Arg => A] = r._2 match {
    case Left(x) => comb.K_[A, Arg](x)(r._1, argi)
    case Right(x) => x
  }

  lazy val in: repr[Arg] = (argi, Right(comb.I(argi)))

  def rconv[A: Info](x: Repr[A]): repr[A] = (implicitly[Info[A]], Left(x))
}
