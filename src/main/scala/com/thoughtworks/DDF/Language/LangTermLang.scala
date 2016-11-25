package com.thoughtworks.DDF.Language

import scalaz.Isomorphism._
import scalaz.NaturalTransformation

trait LangTermLang extends Lang[LangInfoG, LangTerm] with IsoLang[InterLangInfoG, LangInfoG, InterLangTerm, LangTerm] {
  override def infoIso = new (InterLangInfoG <~> LangInfoG) {
    override def to = new NaturalTransformation[InterLangInfoG, LangInfoG] {
      override def apply[A](fa: InterLangInfoG[A]): LangInfoG[A] = new LangInfoG[A] {
        override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = fa(lang)
      }
    }

    override def from = new NaturalTransformation[LangInfoG, InterLangInfoG] {
      override def apply[A](fa: LangInfoG[A]): InterLangInfoG[A] = new InterLangInfoG[A] {
        override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = fa(InterLang2Lang.apply(lang))
      }
    }
  }

  override def reprIso = new (InterLangTerm <~> LangTerm) {
    override def to = new NaturalTransformation[InterLangTerm, LangTerm] {
      override def apply[A](fa: InterLangTerm[A]): LangTerm[A] = new LangTerm[A] {
        override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[A] = fa(lang)
      }
    }

    override def from = new NaturalTransformation[LangTerm, InterLangTerm] {
      override def apply[A](fa: LangTerm[A]): InterLangTerm[A] = new InterLangTerm[A] {
        override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = fa(InterLang2Lang.apply(lang))
      }
    }
  }

  override def l: Lang[InterLangInfoG, InterLangTerm] = InterLangTermLang
}

object LangTermLang extends LangTermLang