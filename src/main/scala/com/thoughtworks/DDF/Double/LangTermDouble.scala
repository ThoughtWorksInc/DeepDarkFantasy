package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Bool.LangTermBool
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch.DoubleRI

trait LangTermDouble extends Double[LangInfoG, LangTerm] with LangTermBool {
  override def multD = new RawLangTerm[scala.Double => scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.multD
  }.convert

  override def expD = new RawLangTerm[scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.expD
  }.convert

  override def plusD = new RawLangTerm[scala.Double => scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.plusD
  }.convert

  override def recipD: LangTerm[scala.Double => scala.Double] = new RawLangTerm[scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.recipD
  }.convert

  override def litD: scala.Double => LangTerm[scala.Double] = d => new RawLangTerm[scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[scala.Double] = lang.litD(d)
  }.convert

  override def minusD = new RawLangTerm[scala.Double => scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.minusD
  }.convert

  override def negD = new RawLangTerm[scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.negD
  }.convert

  override def ltD = new RawLangTerm[scala.Double => scala.Double => Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ltD
  }.convert

  override def divD = new RawLangTerm[scala.Double => scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.divD
  }.convert

  override implicit def doubleInfo = new LangInfoG[scala.Double] with DoubleRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[scala.Double] = lang.doubleInfo
  }

  override def sigD = new RawLangTerm[scala.Double => scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sigD
  }.convert
}

object LangTermDouble extends LangTermDouble