package com.thoughtworks.DDF

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.Language.{BEvalLang, Lang, LangTerm, LangTermLang}

trait ImpW[T] {
  ext =>

  type Weight

  val w: Weight

  val exp: LangTerm[Weight => T]

  implicit val bel = BEvalLang.apply

  implicit val ltl = LangTermLang

  implicit val wi = ltl.arrowDomainInfo(ltl.reprInfo(exp(ltl)))

  implicit val ti = ltl.arrowRangeInfo(ltl.reprInfo(exp(ltl)))

  implicit val wl: Loss[Weight] = wi(bel)

  def update[TL](rate: Double, tl: TL)(implicit ti: Loss.Aux[T, TL]): ImpW[T] = {
    val newW = wl.update(w)(rate)(new BEvalArrow {}.aeval(exp(bel)).forward(ext.wl.convert(w)).backward(tl))
    new ImpW[T] {
      override type Weight = ext.Weight

      override val w: ext.Weight = newW

      override val exp: LangTerm[ext.Weight => T] = ext.exp
    }
  }
}

object ImpW {
  def apply[T](expT: LangTerm[T]): ImpW[T] =
    new ImpW[T] {
      override type Weight = scala.Unit

      override val w: scala.Unit = ()

      override val exp: LangTerm[scala.Unit => T] = new LangTerm[scala.Unit => T] {
        override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[scala.Unit => T] =
          lang.K_(expT(lang))(lang.unitInfo)
      }
    }
}