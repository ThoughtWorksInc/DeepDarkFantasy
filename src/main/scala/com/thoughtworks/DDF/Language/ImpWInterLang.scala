package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.ImpW

import scalaz.NaturalTransformation

trait ImpWInterLang extends NTInterLang[InterLangInfoG, InterLangTerm, ImpW] {
  override def NTF: NaturalTransformation[InterLangTerm, ImpW] = new NaturalTransformation[InterLangTerm, ImpW] {
    override def apply[A](fa: InterLangTerm[A]): ImpW[A] = ImpW(fa)
  }

  override def reprInfo[A]: ImpW[A] => InterLangInfoG[A] = _.ti

  override def base: InterLang[InterLangInfoG, InterLangTerm] = InterLangTermInterLang

  def appRich[A, B](f: ImpW[A => B])(x: ImpW[A]): ImpW.Aux[B, (f.Weight, x.Weight)] =
    new ImpW[B] {
      override type Weight = (f.Weight, x.Weight)

      override val w: Weight = (f.w, x.w)

      override val exp: InterLangTerm[Weight => B] = {
        val l = InterLangTermInterLang
        l.S__(l.B__(f.exp)(l.zeroth(f.wi, x.wi)))(l.B__(x.exp)(l.first(f.wi, x.wi)))
      }
    }

  override def app[A, B]: ImpW[A => B] => ImpW[A] => ImpW[B] = f => x => appRich[A, B](f)(x)
}

object ImpWInterLang extends ImpWInterLang