package com.thoughtworks.DDF.Language

import scalaz.NaturalTransformation

trait LangTermLangInfo[R[_]] extends
  LangInfo[LangInfoG, R] {
  override implicit def productInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[(A, B)] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[(A, B)] =
        lang.productInfo(ai(lang), bi(lang))
    }

  override def productZerothInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[A] = i =>
    new LangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.productZerothInfo(i(lang))
    }

  override def productFirstInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[B] = i =>
    new LangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.productFirstInfo(i(lang))
    }

  override implicit def doubleInfo: LangInfoG[Double] = new LangInfoG[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Double] = lang.doubleInfo
  }

  override implicit def aInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))
    }

  override def domInfo[A, B]: LangInfoG[A => B] => LangInfoG[A] = i =>
    new LangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.domInfo(i(lang))
    }

  override def rngInfo[A, B]: LangInfoG[A => B] => LangInfoG[B] = i =>
    new LangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.rngInfo(i(lang))
    }

  override implicit def boolInfo: LangInfoG[Boolean] = new LangInfoG[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Boolean] = lang.boolInfo
  }

  override implicit def listInfo[A](implicit ai: LangInfoG[A]): LangInfoG[List[A]] = new LangInfoG[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[List[A]] = lang.listInfo(ai(lang))
  }

  override def listElmInfo[A]: LangInfoG[List[A]] => LangInfoG[A] = i => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.listElmInfo(i(lang))
  }

  override implicit def sumInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[Either[A, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Either[A, B]] =
        lang.sumInfo(ai(lang), bi(lang))
    }

  override def sumLeftInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[A] = i =>
    new LangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.sumLeftInfo(i(lang))
    }

  override def sumRightInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[B] = i =>
    new LangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.sumRightInfo(i(lang))
    }

  override implicit def unitInfo: LangInfoG[Unit] = new LangInfoG[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Unit] = lang.unitInfo
  }

  override implicit def optionInfo[A](implicit ai: LangInfoG[A]): LangInfoG[Option[A]] = new LangInfoG[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Option[A]] = lang.optionInfo(ai(lang))
  }

  override def optionElmInfo[A]: LangInfoG[Option[A]] => LangInfoG[A] = i => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.optionElmInfo(i(lang))
  }
}

object LangTermLangInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, LangInfoG] => LangTermLangInfo[Repr] =
    nt => new LangTermLangInfo[Repr] {
      override def reprInfo[A]: Repr[A] => LangInfoG[A] = nt.apply
    }
}