package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextBase[Info[_], Repr[_], Arg] {
  implicit def argi: Info[Arg]

  implicit def ski: SKIRepr[Info, Repr]

  def rconv[X]: Repr[X] => Either[Repr[X], Repr[Arg => X]] = x => Left(x)

  def iconv[X]: Info[X] => Info[Arg => X] = x => ski.ArrowInfo[Arg, X](argi, x)

  def in = Right(ski.I)

  def collapse[X]: Either[Repr[X], Repr[Arg => X]] => Repr[Arg => X] = {
    case Right(x) => x
    case Left(x) => lift(x)
  }

  def lift[X]: Repr[X] => Repr[Arg => X] = r => ski.app(ski.K[X, Arg](ski.ReprInfo(r), argi))(r)
}
