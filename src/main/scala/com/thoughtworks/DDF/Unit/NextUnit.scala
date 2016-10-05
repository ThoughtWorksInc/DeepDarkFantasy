package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.Combinators.SKIRepr
import com.thoughtworks.DDF.InfoBase.NextInfoBase
import com.thoughtworks.DDF.NextBase

trait NextUnit[Info[_], Repr[_], Arg] extends
  UnitRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] with
  NextInfoBase[Info, Repr, Arg] {
  implicit def base: UnitRepr[Info, Repr]

  override def mkUnit = rconv(base.mkUnit)

  override implicit def unitInfo: Info[Arg => Unit] = iconv(base.unitInfo)
}

object NextUnit {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            unit: UnitRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg]) = new NextUnit[Info, Repr, Arg] {
    override implicit def base: UnitRepr[Info, Repr] = unit

    override implicit def ski: SKIRepr[Info, Repr] = skir

    override implicit def argi: Info[Arg] = arg
  }
}
