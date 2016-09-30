package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.{NaiveNextComb, SKILang}
import com.thoughtworks.DDF.Unit.NaiveNextUnit

//Exists only for pedantic purpose. Use NextLang instead.
trait NaiveNextCombUnit[Info[_], Repr[_], Arg] extends
  CombUnit[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextComb[Info, Repr, Arg] with
  NaiveNextUnit[Info, Repr, Arg] {
  implicit def base: CombUnit[Info, Repr]
}

object NaiveNextCombUnit {
  def apply[Info[_], Repr[_], Arg](implicit cu: CombUnit[Info, Repr], arg: Info[Arg]) =
    new NaiveNextCombUnit[Info, Repr, Arg] {
      override implicit def base: CombUnit[Info, Repr] = cu

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKILang[Info, Repr] = cu
    }
}