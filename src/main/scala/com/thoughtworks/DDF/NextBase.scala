package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.SKI

class Next[Info[_], Repr[_], Arg] {
  final type info[X] = Info[Arg => X]

  final type repr[X] = Either[Repr[X], Repr[Arg => X]]
}

trait NextBase[Info[_], Repr[_], Arg] {
  type info[X] = Next[Info, Repr, Arg]#info[X]

  type repr[X] = Next[Info, Repr, Arg]#repr[X]

  implicit def argi: Info[Arg]

  implicit def ski: SKI[Info, Repr]

  def rconv[X]: Repr[X] => repr[X] = x => Left(x)

  def iconv[X]: Info[X] => info[X] = x => ski.arrowInfo(argi, x)

  def convi[X]: info[X] => Info[X] = x => ski.arrowRangeInfo(x)

  def in: repr[Arg] = Right(ski.I)

  def collapse[X]: Next[Info, Repr, Arg]#repr[X] => Repr[Arg => X] = {
    case Left(x) => lift(x)
    case Right(x) => x
  }

  def lift[X]: Repr[X] => Repr[Arg => X] = r => ski.app(ski.K[X, Arg](ski.reprInfo(r), argi))(r)
}
