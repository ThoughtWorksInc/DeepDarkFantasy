package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.Arr

trait ITE[Info[_], Repr[_]] extends Arr[Info, Repr] with BoolInfo[Info, Repr] {
  def ite[A : Info] : Repr[Boolean => A => A => A]

  def ite_[A : Info](b : Repr[Boolean]) : Repr[A => A => A] = app[Boolean, A => A => A](ite[A])(b)

  def ite__[A : Info](b : Repr[Boolean])(l : Repr[A]) = app(ite_[A](b))(l)

  def ite___[A : Info](b : Repr[Boolean])(l : Repr[A])(r : Repr[A]) : Repr[A] = app(ite__(b)(l))(r)
}
