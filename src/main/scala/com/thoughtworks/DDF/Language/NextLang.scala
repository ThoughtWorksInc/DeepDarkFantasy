package com.thoughtworks.DDF.Language
import com.thoughtworks.DDF.Combinators.Comb

trait NextLang[Info[_], Repr[_], Arg] extends
  Lang[Info, Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  InterLang2Lang[Info, Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] {
  override implicit def comb: Comb[Info, Repr] = il

  override val i = NextInterLang.apply[Info, Repr, Arg](il, argi)

  def il: InterLang[Info, Repr]
}

object NextLang {
  implicit def apply[Info[_], Repr[_], Arg](implicit l: InterLang[Info, Repr], ai: Info[Arg]) =
    new NextLang[Info, Repr, Arg] {
      override def il: InterLang[Info, Repr] = l

      override val argi = ai
    }
}