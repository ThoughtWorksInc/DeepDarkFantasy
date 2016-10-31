package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ArrMin

trait Nil[Info[_], Repr[_]] extends ListInfo[Info, Repr] with ArrMin[Info, Repr] {
  def nil[A](implicit ai: Info[A]): Repr[scala.List[A]]
}
