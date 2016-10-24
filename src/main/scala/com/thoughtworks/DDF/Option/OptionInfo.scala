package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ArrowInfo

trait OptionInfo[Info[_], Repr[_]] extends ArrowInfo[Info, Repr] {
  implicit def optionInfo[A](implicit ai: Info[A]): Info[scala.Option[A]]

  def optionElmInfo[A]: Info[scala.Option[A]] => Info[A]
}
