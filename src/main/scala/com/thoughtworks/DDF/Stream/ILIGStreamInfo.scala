package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}
import com.thoughtworks.DDF.Top.ILIGTopInfo

import scalaz.NaturalTransformation

trait ILIGStreamInfo[R[_]] extends
  StreamInfo[InterLangInfoG, R] with
  ILIGArrInfo[R] with
  ILIGTopInfo[R] {
  override implicit def streamInfo[A](implicit ai: InterLangInfoG[A]) =
    new InterLangInfoG[scala.Stream[A]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.streamInfo(ai(lang))
    }

  override def streamElmInfo[A]: InterLangInfoG[scala.Stream[A]] => InterLangInfoG[A] = sa =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.streamElmInfo(sa(lang))
    }
}

object ILIGStreamInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGStreamInfo[Repr] =
    nt => new ILIGStreamInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}