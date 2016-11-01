package com.thoughtworks.DDF.Language

import scalaz.NaturalTransformation

trait LangTermLangInfo[R[_]] extends
  LangInfo[InterLangInfoG, R] {
  override implicit def prodInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[(A, B)] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[(A, B)] =
        lang.prodInfo(ai(lang), bi(lang))
    }

  override def prodZroInfo[A, B]: InterLangInfoG[(A, B)] => InterLangInfoG[A] = i =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.prodZroInfo(i(lang))
    }

  override def prodFstInfo[A, B]: InterLangInfoG[(A, B)] => InterLangInfoG[B] = i =>
    new InterLangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.prodFstInfo(i(lang))
    }

  override implicit def doubleInfo: InterLangInfoG[Double] = new InterLangInfoG[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Double] = lang.doubleInfo
  }

  override implicit def aInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))
    }

  override def domInfo[A, B]: InterLangInfoG[A => B] => InterLangInfoG[A] = i =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.domInfo(i(lang))
    }

  override def rngInfo[A, B]: InterLangInfoG[A => B] => InterLangInfoG[B] = i =>
    new InterLangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.rngInfo(i(lang))
    }

  override implicit def boolInfo: InterLangInfoG[Boolean] = new InterLangInfoG[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Boolean] = lang.boolInfo
  }

  override implicit def listInfo[A](implicit ai: InterLangInfoG[A]): InterLangInfoG[List[A]] = new InterLangInfoG[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[List[A]] = lang.listInfo(ai(lang))
  }

  override def listElmInfo[A]: InterLangInfoG[List[A]] => InterLangInfoG[A] = i => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.listElmInfo(i(lang))
  }

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

  override implicit def unitInfo: InterLangInfoG[Unit] = new InterLangInfoG[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Unit] = lang.unitInfo
  }

  override implicit def optionInfo[A](implicit ai: InterLangInfoG[A]): InterLangInfoG[Option[A]] = new InterLangInfoG[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Option[A]] = lang.optionInfo(ai(lang))
  }

  override def optionElmInfo[A]: InterLangInfoG[Option[A]] => InterLangInfoG[A] = i => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.optionElmInfo(i(lang))
  }
}

object LangTermLangInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => LangTermLangInfo[Repr] =
    nt => new LangTermLangInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}