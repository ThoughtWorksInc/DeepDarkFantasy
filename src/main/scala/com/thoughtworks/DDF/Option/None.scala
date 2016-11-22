package com.thoughtworks.DDF.Option

trait None[Info[_], Repr[_]] extends OptionInfo[Info, Repr] {
  def none[A](implicit ai: Info[A]): Repr[scala.Option[A]]
}
