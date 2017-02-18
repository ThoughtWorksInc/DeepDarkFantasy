package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language.{Lang, LangInfoG, LangTerm, RawLangTerm}

trait LangTermComb extends Comb with LangTermArr {
  override def K[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => B => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.K[A, B](ai(lang), bi(lang))
  }.convert

  override def I[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.I[A](ai(lang))
  }.convert

  override def Let[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => (A => B) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Let(ai(lang), bi(lang))
  }.convert

  override def C[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(A => B => C) => B => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.C(ai(lang), bi(lang), ci(lang))
    }.convert

  override def S[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(A => B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.S(ai(lang), bi(lang), ci(lang))
    }.convert

  override def W[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[(A => A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.W[A, B](ai(lang), bi(lang))
  }.convert

  override def App[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[(A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.App(ai(lang), bi(lang))
  }.convert

  override def B[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.B(ai(lang), bi(lang), ci(lang))
    }.convert

  override def Y[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[((A => B) => A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Y[A, B](ai(lang), bi(lang))
  }.convert
}

object LangTermComb extends LangTermComb