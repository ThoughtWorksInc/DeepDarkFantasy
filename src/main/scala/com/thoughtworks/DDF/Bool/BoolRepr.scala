package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait BoolRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] with BoolInfo[Info, Repr] {
  def litB: Boolean => Repr[Boolean]

  def ite[A](implicit ai: Info[A]): Repr[Boolean => A => A => A]
}
