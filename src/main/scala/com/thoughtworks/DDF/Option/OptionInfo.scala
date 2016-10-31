package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ArrInfo

trait OptionInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def optionInfo[A](implicit ai: Info[A]): Info[scala.Option[A]]

  def optionElmInfo[A]: Info[scala.Option[A]] => Info[A]
}
