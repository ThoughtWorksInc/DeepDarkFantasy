package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.Product.LangTermProd
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait LangTermList extends com.thoughtworks.DDF.List.List[LangInfoG, LangTerm] with LangTermProd {
  override def reverse[A](implicit ai: LangInfoG[A]) = new RawLangTerm[scala.List[A] => scala.List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.reverse[A](ai(lang))
  }.convert

  override def foldLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B => A) => A => scala.List[B] => A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldLeft[A, B](ai(lang), bi(lang))
    }.convert

  override def cons[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => scala.List[A] => scala.List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.cons[A](ai(lang))
  }.convert

  override implicit def listInfo[A](implicit ai: LangInfoG[A]) =
    new LangInfoG[scala.List[A]] with ListRI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[scala.List[A]] =
        lang.listInfo(ai(lang))

      override def tmr: tm.ret = ai
    }

  override def listElmInfo[A]: LangInfoG[scala.List[A]] => LangInfoG[A] = _.get(LM[LangInfoGMatch, LangInfoG, A])

  override def nil[A](implicit ai: LangInfoG[A]) = new RawLangTerm[scala.List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[scala.List[A]] = lang.nil(ai(lang))
  }.convert

  override def listMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[scala.List[A] => B => (A => scala.List[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMatch(ai(lang), bi(lang))
    }.convert

  override def foldRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B => B) => B => scala.List[A] => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldRight[A, B](ai(lang), bi(lang))
    }.convert

  override def listZip[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[scala.List[A] => scala.List[B] => scala.List[(A, B)]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listZip[A, B](ai(lang), bi(lang))
    }.convert

  override def scanLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(B => A => B) => B => scala.List[A] => scala.List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanLeft(ai(lang), bi(lang))
    }.convert

  override def listMap[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B) => scala.List[A] => scala.List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMap(ai(lang), bi(lang))
    }.convert

  override def scanRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B => B) => B => scala.List[A] => scala.List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanRight(ai(lang), bi(lang))
    }.convert
}

object LangTermList extends LangTermList