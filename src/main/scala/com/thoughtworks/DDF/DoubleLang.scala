package com.thoughtworks.DDF

trait DoubleLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def DoubleInfo: Info[Double]

  def LitD: Double => Repr[Double]

  def PlusD: Repr[Double => Double => Double]

  def MultD: Repr[Double => Double => Double]
}
