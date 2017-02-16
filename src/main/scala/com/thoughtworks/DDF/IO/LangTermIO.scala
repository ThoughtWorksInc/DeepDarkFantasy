package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.LangTermDouble
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch._
import com.thoughtworks.DDF.Top.LangTermTop

trait LangTermIO extends IO[LangInfoG, LangTerm] with LangTermDouble with LangTermTop {
  override implicit def IOInfo[A](implicit ai: LangInfoG[A]): LangInfoG[IO[A]] =
    new LangInfoG[IO[A]] with IORI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[IO[A]] = lang.IOInfo(ai(lang))

      override def tmr: tm.ret = ai
    }

  override def IOElmInfo[A]: LangInfoG[IO[A]] => LangInfoG[A] = _.get(IOM[LangInfoGMatch, LangInfoG, A])

  override def putDouble = new RawLangTerm[scala.Double => IO[Unit]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.putDouble
  }.convert

  override def getDouble: LangTerm[IO[Double]] = new RawLangTerm[IO[Double]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[IO[Double]] = lang.getDouble
  }.convert

  override def IOBind[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[IO[A] => (A => IO[B]) => IO[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.IOBind(ai(lang), bi(lang))
    }.convert

  override def IORet[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.IORet[A](ai(lang))
  }.convert
}

object LangTermIO extends LangTermIO
