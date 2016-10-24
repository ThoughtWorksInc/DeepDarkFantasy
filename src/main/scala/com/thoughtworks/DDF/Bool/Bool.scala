package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.Arrow

trait Bool[Info[_], Repr[_]] extends Arrow[Info, Repr] with BoolInfo[Info, Repr] {
  def litB: Boolean => Repr[Boolean]

  def ite[A](implicit ai: Info[A]): Repr[Boolean => A => A => A]
}
