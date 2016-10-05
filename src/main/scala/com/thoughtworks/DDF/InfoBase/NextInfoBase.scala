package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.Combinators.SKIRepr
import com.thoughtworks.DDF.NextBase

trait NextInfoBase[Info[_], Repr[_], Arg] extends
  InfoBase[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]]  with
  NextBase[Info, Repr, Arg] {
  implicit def base: InfoBase[Info, Repr]

  override def reprInfo[A]: Either[Repr[A], Repr[Arg => A]] => Info[Arg => A] = {
    case Left(x) => iconv(base.reprInfo(x))
    case Right(x) => base.reprInfo(x)
  }
}

object NextInfoBase {
  implicit def apply[Info[_], Repr[_], Arg](implicit skir: SKIRepr[Info, Repr], arg: Info[Arg]) =
    new NextInfoBase[Info, Repr, Arg] {
      override implicit def base: InfoBase[Info, Repr] = ski

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = skir
  }
}