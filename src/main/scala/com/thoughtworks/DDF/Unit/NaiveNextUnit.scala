package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.Combinators.SKILang
import com.thoughtworks.DDF.NaiveNextBase

trait NaiveNextUnit[Info[_], Repr[_], Arg] extends
  UnitLang[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with NaiveNextBase[Info, Repr, Arg] {
  implicit def base: UnitLang[Info, Repr]

  override def mkUnit: Repr[Arg => Unit] = rconv(base.mkUnit)

  override implicit def UnitInfo: Info[Arg => Unit] = iconv(base.UnitInfo)
}

object NaiveNextUnit {
  implicit def apply[Info[_], Repr[_], Arg]
  (implicit unitl: UnitLang[Info, Repr], skil: SKILang[Info, Repr], arg: Info[Arg]) =
    new NaiveNextUnit[Info, Repr, Arg] {
      override implicit def base: UnitLang[Info, Repr] = unitl

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKILang[Info, Repr] = skil
  }
}