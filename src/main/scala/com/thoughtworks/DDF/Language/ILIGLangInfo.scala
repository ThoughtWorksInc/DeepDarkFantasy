package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.ILIGBoolInfo
import com.thoughtworks.DDF.Double.ILIGDoubleInfo
import com.thoughtworks.DDF.List.ILIGListInfo
import com.thoughtworks.DDF.Product.ILIGProdInfo

import scalaz.NaturalTransformation

trait ILIGLangInfo[R[_]] extends
  LangInfo[InterLangInfoG, R] with
  ILIGProdInfo[R] with
  ILIGDoubleInfo[R] with
  ILIGBoolInfo[R] with
  ILIGListInfo[R] {
  override implicit def sumInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[Either[A, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Either[A, B]] =
        lang.sumInfo(ai(lang), bi(lang))
    }

  override def sumLeftInfo[A, B]: InterLangInfoG[Either[A, B]] => InterLangInfoG[A] = i =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.sumLeftInfo(i(lang))
    }

  override def sumRightInfo[A, B]: InterLangInfoG[Either[A, B]] => InterLangInfoG[B] = i =>
    new InterLangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.sumRightInfo(i(lang))
    }

  override implicit def topInfo: InterLangInfoG[Unit] = new InterLangInfoG[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Unit] = lang.topInfo
  }

  override implicit def optionInfo[A](implicit ai: InterLangInfoG[A]) = new InterLangInfoG[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.optionInfo(ai(lang))
  }

  override def optionElmInfo[A]: InterLangInfoG[Option[A]] => InterLangInfoG[A] = i => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.optionElmInfo(i(lang))
  }

  override def IOInfo[A](implicit ai: InterLangInfoG[A]) = new InterLangInfoG[IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.IOInfo(ai(lang))
  }

  override def IOElmInfo[A]: InterLangInfoG[IO[A]] => InterLangInfoG[A] = ioa => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.IOElmInfo(ioa(lang))
  }

  override implicit def streamInfo[A](implicit ai: InterLangInfoG[A]) =
    new InterLangInfoG[Stream[A]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.streamInfo(ai(lang))
    }

  override def streamElmInfo[A]: InterLangInfoG[Stream[A]] => InterLangInfoG[A] = sa =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.streamElmInfo(sa(lang))
    }

  override implicit def botInfo: InterLangInfoG[Nothing] =
    new InterLangInfoG[Nothing] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Nothing] = lang.botInfo
    }
}

object ILIGLangInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGLangInfo[Repr] =
    nt => new ILIGLangInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}