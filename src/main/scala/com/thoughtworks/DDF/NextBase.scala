package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextBase[Info[_], Repr[_], Arg] {
  implicit def argi: Info[Arg]

  implicit def ski: SKIRepr[Info, Repr]

  def rconv[X]: Repr[X] => Either[Repr[X], Repr[Arg => X]] = x => Left(x)

  def iconv[X]: Info[X] => Info[Arg => X] = x => ski.arrowInfo(argi, x)

  def convi[X]: Info[Arg => X] => Info[X] = x => ski.arrowRangeInfo(x)

  def in = Right(ski.I)

  def collapse[X]: Either[Repr[X], Repr[Arg => X]] => Repr[Arg => X] = {
    case Left(x) => lift(x)
    case Right(x) => x
  }

  def lift[X]: Repr[X] => Repr[Arg => X] = r => ski.app(ski.K[X, Arg](ski.reprInfo(r), argi))(r)
}
