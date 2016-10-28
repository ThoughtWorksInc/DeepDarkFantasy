package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Loss.Aux
import com.thoughtworks.DDF.{ImpW, ImpWLog, NoInfo}

import scalaz.NaturalTransformation

trait ImpWLogLang extends NTLang[LangInfoG, ImpW, ImpWLog] {
  override def base: Lang[LangInfoG, ImpW] = ImpWLang

  def defLogHandler: Seq[String] => Seq[String]

  override def NTF: NaturalTransformation[ImpW, ImpWLog] = new NaturalTransformation[ImpW, ImpWLog] {
    override def apply[A](fa: ImpW[A]): ImpWLog[A] = new ImpWLog[A] {
      override def update[XL](rate: Double, xl: XL)(implicit ti: Aux[A, XL]): ImpWLog[A] = ???

      override val exp: ImpW[A] = fa
    }
  }

  override def app[A, B]: ImpWLog[A => B] => ImpWLog[A] => ImpWLog[B] = ???

  override def reprInfo[A]: ImpWLog[A] => LangInfoG[A] = _.exp.ti
}

object ImpWLogLang {
  def apply(h: Seq[String] => Seq[String]) = new ImpWLogLang {
    override def defLogHandler: Seq[String] => Seq[String] = h
  }
}