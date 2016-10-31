package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.Arr

trait Bool[Info[_], Repr[_]] extends Arr[Info, Repr] with BoolInfo[Info, Repr] {
  def litB: Boolean => Repr[Boolean]

  def ite[A](implicit ai: Info[A]): Repr[Boolean => A => A => A]

  def ite_[A](b: Repr[Boolean])(implicit ai: Info[A]): Repr[A => A => A] = app(ite(ai))(b)

  def ite__[A](b: Repr[Boolean])(l: Repr[A]) = app(ite_(b)(reprInfo(l)))(l)

  def ite___[A]: Repr[Boolean] => Repr[A] => Repr[A] => Repr[A] = b => l => app(ite__(b)(l))
}
