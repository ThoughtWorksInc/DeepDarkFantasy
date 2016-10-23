package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.Combinators.SKI
import com.thoughtworks.DDF.NaiveNextBase
import com.thoughtworks.DDF.InfoBase.NaiveNextInfoBase

trait NaiveNextUnit[Info[_], Repr[_], Arg] extends
  Unit[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextBase[Info, Repr, Arg] with
  NaiveNextInfoBase[Info, Repr, Arg] {
  implicit def base: Unit[Info, Repr]

  override def mkUnit: Repr[Arg => Unit] = rconv(base.mkUnit)

  override implicit def unitInfo: Info[Arg => Unit] = iconv(base.unitInfo)
}

object NaiveNextUnit {
  implicit def apply[Info[_], Repr[_], Arg]
  (implicit unitl: Unit[Info, Repr], skil: SKI[Info, Repr], arg: Info[Arg]) =
    new NaiveNextUnit[Info, Repr, Arg] {
      override implicit def base: Unit[Info, Repr] = unitl

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKI[Info, Repr] = skil
  }
}