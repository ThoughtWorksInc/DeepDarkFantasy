package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.ImpW

import scalaz.NaturalTransformation

trait ImpWLang extends NTLang[LangInfoG, LangTerm, ImpW] {
  override def NTF: NaturalTransformation[LangTerm, ImpW] = new NaturalTransformation[LangTerm, ImpW] {
    override def apply[A](fa: LangTerm[A]): ImpW[A] = ImpW(fa)
  }

  override def reprInfo[A]: ImpW[A] => LangInfoG[A] = _.ti

  override def base: Lang[LangInfoG, LangTerm] = LangTermLang

  override def app[A, B]: ImpW[A => B] => ImpW[A] => ImpW[B] = f => x => new ImpW[B] {
    override type Weight = (f.Weight, x.Weight)

    override val w: Weight = (f.w, x.w)

    override val exp: LangTerm[Weight => B] = {
      val l = LangTermLang
      l.S__(l.B__(f.exp)(l.zeroth(f.wi, x.wi)))(l.B__(x.exp)(l.first(f.wi, x.wi)))
    }
  }
}

object ImpWLang extends ImpWLang