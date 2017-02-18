package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch._
import com.thoughtworks.DDF.Top.LangTermTop

trait LangTermStream extends Stream with LangTermTop with LangTermArr {
  override def streamMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[scala.Stream[A] => B => (A => scala.Stream[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamMatch(ai(lang), bi(lang))
    }.convert

  override def streamNil[A](implicit ai: LangInfoG[A]) = new RawLangTerm[scala.Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[scala.Stream[A]] =
      lang.streamNil(ai(lang))
  }.convert

  override def streamCons[A](implicit ai: LangInfoG[A]) =
    new RawLangTerm[A => (Unit => scala.Stream[A]) => scala.Stream[A]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamCons[A](ai(lang))
    }.convert

  override implicit def streamInfo[A](implicit ai: LangInfoG[A]) =
    new LangInfoG[scala.Stream[A]] with StreamRI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]):
      Info[scala.Stream[A]] = lang.streamInfo(ai(lang))

      override def tmr: tm.ret = ai
    }

  override def streamElmInfo[A]: LangInfoG[scala.Stream[A]] => LangInfoG[A] = _.get(StM[LangInfoGMatch, LangInfoG, A])
}

object LangTermStream extends LangTermStream