package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch.{StreamRI, _}
import com.thoughtworks.DDF.Top.LangTermTop

trait LangTermStream extends com.thoughtworks.DDF.Stream.Stream[LangInfoG, LangTerm] with LangTermTop with LangTermArr {
  override def streamMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Stream[A] => B => (A => Stream[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamMatch(ai(lang), bi(lang))
    }.convert

  override def streamNil[A](implicit ai: LangInfoG[A]): LangTerm[Stream[A]] = new RawLangTerm[Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Stream[A]] = lang.streamNil(ai(lang))
  }.convert

  override def streamCons[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => (Unit => Stream[A]) => Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamCons[A](ai(lang))
  }.convert

  override implicit def streamInfo[A](implicit ai: LangInfoG[A]): LangInfoG[Stream[A]] =
    new LangInfoG[Stream[A]] with StreamRI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Stream[A]] = lang.streamInfo(ai(lang))

      override def tmr: tm.ret = ai
    }

  override def streamElmInfo[A]: LangInfoG[Stream[A]] => LangInfoG[A] = _.get(StM[LangInfoGMatch, LangInfoG, A])
}

object LangTermStream extends LangTermStream