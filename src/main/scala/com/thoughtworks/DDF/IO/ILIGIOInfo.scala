package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGIOInfo[R[_]] extends
  IOInfo[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override def IOInfo[A](implicit ai: InterLangInfoG[A]) = new InterLangInfoG[IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.IOInfo(ai(lang))
  }

  override def IOElmInfo[A]: InterLangInfoG[IO[A]] => InterLangInfoG[A] = ioa => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.IOElmInfo(ioa(lang))
  }
}

object ILIGIOInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGIOInfo[Repr] =
    nt => new ILIGIOInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}