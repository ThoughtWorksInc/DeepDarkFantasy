package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.{NaiveNextComb, SKIRepr}
import com.thoughtworks.DDF.Unit.NaiveNextUnit

//Exists only for pedantic purpose. Use NextLang instead.
trait NaiveNextCombUnitL[Info[_], Repr[_], Arg] extends
  CombUnitL[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextComb[Info, Repr, Arg] with
  NaiveNextUnit[Info, Repr, Arg] {
  implicit def base: CombUnitL[Info, Repr]
}

object NaiveNextCombUnitL {
  implicit def apply[Info[_], Repr[_], Arg](implicit cu: CombUnitL[Info, Repr], arg: Info[Arg]) =
    new NaiveNextCombUnitL[Info, Repr, Arg] {
      override implicit def base: CombUnitL[Info, Repr] = cu

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = cu
    }
}