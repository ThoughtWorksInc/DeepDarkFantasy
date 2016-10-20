package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Bool.NextBool
import com.thoughtworks.DDF.Combinators.{NextComb, SKIRepr}
import com.thoughtworks.DDF.Double.NextDouble
import com.thoughtworks.DDF.List.NextList
import com.thoughtworks.DDF.NextBase
import com.thoughtworks.DDF.Option.NextOption
import com.thoughtworks.DDF.Product.NextProduct
import com.thoughtworks.DDF.Sum.NextSum
import com.thoughtworks.DDF.Unit.NextUnit

trait NextLangL[Info[_], Repr[_], Arg] extends
  LangL[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] with
  NextComb[Info, Repr, Arg] with
  NextDouble[Info, Repr, Arg] with
  NextProduct[Info, Repr, Arg] with
  NextOption[Info, Repr, Arg] with
  NextSum[Info, Repr, Arg] with
  NextList[Info, Repr, Arg] with
  NextUnit[Info, Repr, Arg] with
  NextBool[Info, Repr, Arg] {
  implicit def base: LangL[Info, Repr]
}

object NextLangL {
  implicit def apply[Info[_], Repr[_], Arg](implicit lang: LangL[Info, Repr], arg: Info[Arg]):
  LangL[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with NextBase[Info, Repr, Arg] =
    new NextLangL[Info, Repr, Arg] {
      override implicit def base: LangL[Info, Repr] = lang

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = lang
  }
}
