package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.Combinators.SKIRepr
import com.thoughtworks.DDF.NaiveNextBase
import com.thoughtworks.DDF.InfoB.NaiveNextInfoB

trait NaiveNextUnit[Info[_], Repr[_], Arg] extends
  UnitRepr[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextBase[Info, Repr, Arg] with
  NaiveNextInfoB[Info, Repr, Arg] {
  implicit def base: UnitRepr[Info, Repr]

  override def mkUnit: Repr[Arg => Unit] = rconv(base.mkUnit)

  override implicit def unitInfo: Info[Arg => Unit] = iconv(base.unitInfo)
}

object NaiveNextUnit {
  implicit def apply[Info[_], Repr[_], Arg]
  (implicit unitl: UnitRepr[Info, Repr], skil: SKIRepr[Info, Repr], arg: Info[Arg]) =
    new NaiveNextUnit[Info, Repr, Arg] {
      override implicit def base: UnitRepr[Info, Repr] = unitl

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = skil
  }
}