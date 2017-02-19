package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.InterLang
import com.thoughtworks.DDF.Top.ILIGTopType

import scalaz.NaturalTransformation

trait ILIGStreamType[R[_]] extends
  StreamType[InterLangInfoG, R] with
  ILIGArrInfo[R] with
  ILIGTopType[R] {
  override implicit def streamInfo[A](implicit ai: InterLangInfoG[A]) =
    new InterLangInfoG[scala.Stream[A]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.streamInfo(ai(lang))
    }

  override def streamElmInfo[A]: InterLangInfoG[scala.Stream[A]] => InterLangInfoG[A] = sa =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.streamElmInfo(sa(lang))
    }
}

object ILIGStreamType {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGStreamType[Repr] =
    nt => new ILIGStreamType[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}