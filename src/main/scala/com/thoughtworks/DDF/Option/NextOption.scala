package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.NextArrow
import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextOption[Info[_], Repr[_], Arg] extends
  OptionRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextArrow[Info, Repr, Arg] {
  implicit def base: OptionRepr[Info, Repr]

  override implicit def optionInfo[A](implicit ai: Info[Arg => A]) = iconv(base.optionInfo(convi(ai)))

  override def optionElmInfo[A] = x => iconv(base.optionElmInfo(convi(x)))

  override def none[A](implicit ai: Info[Arg => A]) = rconv(base.none(convi(ai)))

  override def some[A](implicit ai: Info[Arg => A]) = rconv(base.some(convi(ai)))

  override def optionMatch[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.optionMatch(convi(ai), convi(bi)))
}

object NextOption {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            opt: OptionRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg]) =
    new NextOption[Info, Repr, Arg] {
      override def base: OptionRepr[Info, Repr] = opt

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = skir
  }
}